package vm

import (
	"fmt"
	"lark/code"
	"lark/compiler"
	"lark/object"
)

const STACK_SIZE = 2048
const GLOBAL_SIZE = 65536
const MAX_FRAMES = 1024

var True = &object.Boolean{Value: true}
var False = &object.Boolean{Value: false}
var Null = &object.Null{}

type VM struct {
	constants []object.Object

	stack        []object.Object
	stackPointer int

	globals []object.Object

	frames      []*Frame
	framesIndex int
}

func New(bytecode *compiler.Bytecode) *VM {
	mainFn := &object.CompiledFunction{Instructions: bytecode.Instructions}
	mainClosure := &object.Closure{Fn: mainFn}
	mainFrame := NewFrame(mainClosure, 0)

	frames := make([]*Frame, MAX_FRAMES)
	frames[0] = mainFrame

	return &VM{
		constants:    bytecode.Constants,
		stack:        make([]object.Object, STACK_SIZE),
		stackPointer: 0,
		globals:      make([]object.Object, GLOBAL_SIZE),
		frames:       frames,
		framesIndex:  1,
	}
}

func NewWithGlobalStore(bytecode *compiler.Bytecode, s []object.Object) *VM {
	vm := New(bytecode)
	vm.globals = s
	return vm
}

func (vm *VM) currentFrame() *Frame {
	return vm.frames[vm.framesIndex-1]
}

func (vm *VM) pushFrame(f *Frame) {
	vm.frames[vm.framesIndex] = f
	vm.framesIndex++
}

func (vm *VM) popFrame() *Frame {
	vm.framesIndex--
	return vm.frames[vm.framesIndex]
}

func (vm *VM) Run() error {

	var instructionPointer int
	var instructions code.Instructions
	var opCode code.OpCode

	for vm.currentFrame().instructionPointer < len(vm.currentFrame().Instructions())-1 {

		vm.currentFrame().instructionPointer++

		instructionPointer = vm.currentFrame().instructionPointer
		instructions = vm.currentFrame().Instructions()
		opCode = code.OpCode(instructions[instructionPointer])

		switch opCode {
		case code.OpConstant:
			opValue := code.ReadUint16(instructions[instructionPointer+1:])
			vm.currentFrame().instructionPointer += 2

			err := vm.push(vm.constants[opValue])
			if err != nil {
				return err
			}

		case code.OpGreaterThan, code.OpEqual, code.OpNotEqual:
			err := vm.executeComparisonOperation(opCode)
			if err != nil {
				return err
			}

		case code.OpAdd, code.OpSub, code.OpMul, code.OpDiv:
			err := vm.executeBinaryOperation(opCode)
			if err != nil {
				return err
			}

		case code.OpTrue:
			err := vm.push(True)
			if err != nil {
				return err
			}

		case code.OpFalse:
			err := vm.push(False)
			if err != nil {
				return err
			}

		case code.OpPop:
			_, err := vm.pop()
			if err != nil {
				return nil
			}

		case code.OpBang:
			err := vm.executeBangOperator()
			if err != nil {
				return err
			}

		case code.OpMinus:
			err := vm.executeMinusOperator()
			if err != nil {
				return err
			}

		case code.OpJump:
			pos := int(code.ReadUint16(instructions[instructionPointer+1:]))
			vm.currentFrame().instructionPointer = pos - 1

		case code.OpJumpNotTruthy:
			pos := int(code.ReadUint16(instructions[instructionPointer+1:]))
			vm.currentFrame().instructionPointer += 2

			condition, _ := vm.pop()
			if !isTruthy(condition) {
				vm.currentFrame().instructionPointer = pos - 1
			}

		case code.OpNull:
			err := vm.push(Null)
			if err != nil {
				return err
			}

		case code.OpSetGlobal:
			globalIndex := code.ReadUint16(instructions[instructionPointer+1:])
			vm.currentFrame().instructionPointer += 2

			identValue, err := vm.pop()
			if err != nil {
				return err
			}

			vm.globals[globalIndex] = identValue

		case code.OpGetGlobal:
			globalIndex := code.ReadUint16(instructions[instructionPointer+1:])
			vm.currentFrame().instructionPointer += 2

			err := vm.push(vm.globals[globalIndex])
			if err != nil {
				return err
			}

		case code.OpArray:
			numElements := int(code.ReadUint16(instructions[instructionPointer+1:]))
			vm.currentFrame().instructionPointer += 2

			array := vm.buildArray(vm.stackPointer-numElements, vm.stackPointer)
			vm.stackPointer = vm.stackPointer - numElements

			err := vm.push(array)
			if err != nil {
				return err
			}

		case code.OpHash:
			numElements := int(code.ReadUint16(instructions[instructionPointer+1:]))
			vm.currentFrame().instructionPointer += 2

			hash, err := vm.buildHash(vm.stackPointer-numElements, vm.stackPointer)
			if err != nil {
				return err
			}

			vm.stackPointer = vm.stackPointer - numElements

			err = vm.push(hash)
			if err != nil {
				return err
			}

		case code.OpIndex:
			index, _ := vm.pop()
			left, _ := vm.pop()

			err := vm.executeIndexExpression(left, index)
			if err != nil {
				return err
			}

		case code.OpCall:
			numArgs := code.ReadUint8(instructions[instructionPointer+1:])
			vm.currentFrame().instructionPointer += 1

			err := vm.executeCall(int(numArgs))
			if err != nil {
				return err
			}

		case code.OpReturnValue:
			returnValue, _ := vm.pop()

			frame := vm.popFrame()
			vm.stackPointer = frame.basePointer - 1

			err := vm.push(returnValue)
			if err != nil {
				return err
			}

		case code.OpReturn:
			frame := vm.popFrame()
			vm.stackPointer = frame.basePointer - 1

			err := vm.push(Null)
			if err != nil {
				return err
			}

		case code.OpSetLocal:
			localIndex := code.ReadUint8(instructions[instructionPointer+1:])
			vm.currentFrame().instructionPointer += 1

			frame := vm.currentFrame()

			vm.stack[frame.basePointer+int(localIndex)], _ = vm.pop()

		case code.OpGetLocal:
			localIndex := code.ReadUint8(instructions[instructionPointer+1:])
			vm.currentFrame().instructionPointer += 1

			frame := vm.currentFrame()

			err := vm.push(vm.stack[frame.basePointer+int(localIndex)])
			if err != nil {
				return err
			}

		case code.OpGetBuiltin:
			builtinIndex := code.ReadUint8(instructions[instructionPointer+1:])
			vm.currentFrame().instructionPointer += 1

			definition := object.Builtins[builtinIndex]

			err := vm.push(definition.Builtin)
			if err != nil {
				return err
			}

		case code.OpClosure:
			constIndex := code.ReadUint16(instructions[instructionPointer+1:])
			numFree := code.ReadUint8(instructions[instructionPointer+3:])

			vm.currentFrame().instructionPointer += 3

			err := vm.pushClosure(int(constIndex), int(numFree))
			if err != nil {
				return err
			}

		case code.OpGetFree:
			freeIndex := code.ReadUint8(instructions[instructionPointer+1:])
			vm.currentFrame().instructionPointer += 1

			currentClosure := vm.currentFrame().closure
			err := vm.push(currentClosure.Free[freeIndex])
			if err != nil {
				return err
			}

		case code.OpCurrentClosure:
			currentClosure := vm.currentFrame().closure
			err := vm.push(currentClosure)
			if err != nil {
				return err
			}

		}
	}

	return nil
}

func (vm *VM) push(arg object.Object) error {
	if vm.stackPointer >= STACK_SIZE {
		return fmt.Errorf("stack overflow")
	}

	vm.stack[vm.stackPointer] = arg
	vm.stackPointer++

	return nil
}

func (vm *VM) pop() (object.Object, error) {
	if vm.stackPointer == 0 {
		return nil, fmt.Errorf("empty stack")
	}

	object := vm.stack[vm.stackPointer-1]
	vm.stackPointer--

	return object, nil
}

func (vm *VM) LastPoppedStackElem() object.Object {
	return vm.stack[vm.stackPointer]
}

func (vm *VM) executeBinaryOperation(op code.OpCode) error {
	rightValue, err := vm.pop()
	if err != nil {
		return err
	}

	leftValue, err := vm.pop()
	if err != nil {
		return err
	}

	leftType := leftValue.Type()
	rightType := rightValue.Type()

	switch {
	case leftType == object.INTEGER_OBJ && rightType == object.INTEGER_OBJ:
		return vm.executeIntegerBinaryOperation(
			op,
			leftValue.(*object.Integer).Value,
			rightValue.(*object.Integer).Value,
		)
	case leftType == object.STRING_OBJ && rightType == object.STRING_OBJ:
		return vm.executeStringBinaryOperation(
			op,
			leftValue.(*object.String).Value,
			rightValue.(*object.String).Value,
		)
	default:
		return fmt.Errorf("unsupported types for binary operation: %s %s",
			leftType, rightType)
	}
}

func (vm *VM) executeIntegerBinaryOperation(op code.OpCode, left int64, right int64) error {
	var value int64
	switch op {
	case code.OpAdd:
		value = left + right
	case code.OpSub:
		value = left - right
	case code.OpDiv:
		value = left / right
	case code.OpMul:
		value = left * right
	default:
		return fmt.Errorf("unknown integer operator: %d", op)
	}

	return vm.push(&object.Integer{Value: value})
}

func (vm *VM) executeStringBinaryOperation(op code.OpCode, left string, right string) error {
	if op != code.OpAdd {
		return fmt.Errorf("unknown string operator: %d", op)
	}

	return vm.push(&object.String{Value: left + right})
}

func (vm *VM) executeComparisonOperation(op code.OpCode) error {
	right, err := vm.pop()
	if err != nil {
		return err
	}

	left, err := vm.pop()
	if err != nil {
		return err
	}

	if left.Type() == object.INTEGER_OBJ && right.Type() == object.INTEGER_OBJ {
		return vm.executeIntegerComparisonOperation(op, left, right)
	}

	switch op {
	case code.OpEqual:
		return vm.push(nativeBoolToBooleanObject(left == right))
	case code.OpNotEqual:
		return vm.push(nativeBoolToBooleanObject(left != right))
	default:
		return fmt.Errorf("unknown operator: %d (%s %s)",
			op, left.Type(), right.Type())
	}
}

func (vm *VM) executeIntegerComparisonOperation(op code.OpCode, left object.Object, right object.Object) error {
	leftVal := left.(*object.Integer).Value
	rightVal := right.(*object.Integer).Value

	switch op {
	case code.OpGreaterThan:
		return vm.push(nativeBoolToBooleanObject(leftVal > rightVal))
	case code.OpEqual:
		return vm.push(nativeBoolToBooleanObject(leftVal == rightVal))
	case code.OpNotEqual:
		return vm.push(nativeBoolToBooleanObject(leftVal != rightVal))
	default:
		return fmt.Errorf("unknown operator: %d", op)
	}
}

func (vm *VM) executeBangOperator() error {
	obj, err := vm.pop()
	if err != nil {
		return err
	}

	switch obj {
	case True:
		return vm.push(False)
	case False:
		return vm.push(True)
	case Null:
		return vm.push(True)
	default:
		return vm.push(False)
	}
}

func (vm *VM) executeMinusOperator() error {
	obj, err := vm.pop()
	if err != nil {
		return err
	}

	if obj.Type() != object.INTEGER_OBJ {
		return fmt.Errorf("unsupported type for negation: %s", obj.Type())
	}

	value := obj.(*object.Integer).Value
	return vm.push(&object.Integer{Value: -value})
}

func nativeBoolToBooleanObject(input bool) *object.Boolean {
	if input {
		return True
	}
	return False
}

func isTruthy(obj object.Object) bool {
	switch obj := obj.(type) {
	case *object.Boolean:
		return obj.Value
	case *object.Null:
		return false
	default:
		return true
	}
}

func (vm *VM) buildArray(startIndex, endIndex int) object.Object {
	elements := make([]object.Object, endIndex-startIndex)

	for i := startIndex; i < endIndex; i++ {
		elements[i-startIndex] = vm.stack[i]
	}

	return &object.Array{Elements: elements}
}

func (vm *VM) buildHash(startIndex, endIndex int) (object.Object, error) {
	hashedPairs := make(map[object.HashKey]object.HashPair)

	for i := startIndex; i < endIndex; i += 2 {
		key := vm.stack[i]
		value := vm.stack[i+1]

		pair := object.HashPair{Key: key, Value: value}

		hashKey, ok := key.(object.Hashable)
		if !ok {
			return nil, fmt.Errorf("unusable as hash key: %s", key.Type())
		}

		hashedPairs[hashKey.HashKey()] = pair
	}

	return &object.Hash{Pairs: hashedPairs}, nil
}

func (vm *VM) executeIndexExpression(left, index object.Object) error {
	switch {
	case left.Type() == object.ARRAY_OBJ && index.Type() == object.INTEGER_OBJ:
		return vm.executeArrayIndex(left, index)
	case left.Type() == object.HASH_OBJ:
		return vm.executeHashIndex(left, index)
	default:
		return fmt.Errorf("index operator not supported: %s", left.Type())
	}
}

func (vm *VM) executeArrayIndex(array, index object.Object) error {
	arrayObject := array.(*object.Array)
	i := index.(*object.Integer).Value
	max := int64(len(arrayObject.Elements) - 1)

	if i < 0 || i > max {
		return vm.push(Null)
	}

	return vm.push(arrayObject.Elements[i])
}

func (vm *VM) executeHashIndex(hash, index object.Object) error {
	hashObject := hash.(*object.Hash)

	key, ok := index.(object.Hashable)
	if !ok {
		return fmt.Errorf("unusable as hash key: %s", index.Type())
	}

	pair, ok := hashObject.Pairs[key.HashKey()]
	if !ok {
		return vm.push(Null)
	}

	return vm.push(pair.Value)
}

func (vm *VM) executeCall(numArgs int) error {
	callee := vm.stack[vm.stackPointer-1-numArgs]
	switch callee := callee.(type) {
	case *object.Closure:
		return vm.callClosure(callee, numArgs)
	case *object.Builtin:
		return vm.callBuiltin(callee, numArgs)
	default:
		return fmt.Errorf("calling non-function and non-built-in")
	}
}

func (vm *VM) callClosure(cl *object.Closure, numArgs int) error {
	if numArgs != cl.Fn.NumParameters {
		return fmt.Errorf("wrong number of arguments: want=%d, got=%d",
			cl.Fn.NumParameters, numArgs)
	}

	frame := NewFrame(cl, vm.stackPointer-numArgs)
	vm.pushFrame(frame)

	vm.stackPointer = frame.basePointer + cl.Fn.NumLocals

	return nil
}

func (vm *VM) callBuiltin(builtin *object.Builtin, numArgs int) error {
	args := vm.stack[vm.stackPointer-numArgs : vm.stackPointer]

	result := builtin.Fn(args...)
	vm.stackPointer = vm.stackPointer - numArgs - 1

	if result != nil {
		vm.push(result)
	} else {
		vm.push(Null)
	}

	return nil
}

func (vm *VM) pushClosure(index int, numFree int) error {
	constant := vm.constants[index]
	function, ok := constant.(*object.CompiledFunction)
	if !ok {
		return fmt.Errorf("not a fucntion %+v", constant)
	}

	free := make([]object.Object, numFree)
	for i := 0; i < numFree; i++ {
		free[i] = vm.stack[vm.stackPointer-numFree+i]
	}

	vm.stackPointer = vm.stackPointer - numFree

	closure := &object.Closure{Fn: function, Free: free}
	return vm.push(closure)
}

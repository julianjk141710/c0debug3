package miniplc0java.navm;

import miniplc0java.analyser.LocalSymbolTable;
import miniplc0java.instruction.Instruction;

import java.util.ArrayList;

public class FunctionDef {
    private int name;
    private int return_slots;
    private int param_slots;
    private int loc_slots;
    private ArrayList<NavmInstruction> body;


//    private LocalSymbolTable localSymbolTable;
//    private LocalVariableStack localVariableStack;
//    private ParamAndReturnValueStack paramAndReturnValueStack;


    public FunctionDef() {
        this.body = new ArrayList<>();
        //this.localVariableStack = new LocalVariableStack();
//        this.paramAndReturnValueStack = new ParamAndReturnValueStack();
    }

//    public FunctionDef(LocalSymbolTable localSymbolTable) {
//        this.localSymbolTable = localSymbolTable;
//
//    }

//    public void setLocalSymbolTable(LocalSymbolTable localSymbolTable) {
//        this.localSymbolTable = localSymbolTable;
//    }
//
//    public LocalSymbolTable getLocalSymbolTable() {
//        return this.localSymbolTable;
//    }

    public int getName() {
        return name;
    }

    public void setName(int name) {
        this.name = name;
    }

    public int getReturn_slots() {
        return return_slots;
    }

    public void setReturn_slots(int return_slots) {
        this.return_slots = return_slots;
    }

    public int getParam_slots() {
        return param_slots;
    }

    public void setParam_slots(int param_slots) {
        this.param_slots = param_slots;
    }

    public int getLoc_slots() {
        return loc_slots;
    }

    public void setLoc_slots(int loc_slots) {
        this.loc_slots = loc_slots;
    }

//    public void addLocalVariableToStack(String localVariableName) {
//        int offset = this.localVariableStack.getLocalStackPointer();
//        this.localVariableStack.addToLocalStack(localVariableName, offset);
//        this.localVariableStack.addLocalStackPointer();
//    }


    public ArrayList<NavmInstruction> getBody() {
        return body;
    }

    public void setBody(ArrayList<NavmInstruction> body) {
        for (NavmInstruction navmInstruction : body) {
            this.body.add(navmInstruction);
        }
    }
//    public void addParamAndReturnValueToStack(String paramOrReturnValue) {
//        int offset = this.paramAndReturnValueStack.getParamReturnStackPointer();
//        this.paramAndReturnValueStack.addToParamReturnStack(paramOrReturnValue, offset);
//        this.paramAndReturnValueStack.addParamReturnStackPointer();
//    }

    public void addStartInstruction(int offsetOfMain, int stackAllocParam) {
        NavmInstruction stackAlloc = new NavmInstruction();
        stackAlloc.setParam(stackAllocParam);
        stackAlloc.setHasParam(true);
        stackAlloc.setOpcode(stackAlloc.getOpcode(Instructions.stackalloc));

        NavmInstruction call = new NavmInstruction();
        call.setParam(offsetOfMain);
        call.setHasParam(true);
        call.setOpcode(call.getOpcode(Instructions.call));

        body.add(stackAlloc);
        body.add(call);

        if (stackAllocParam == 1) {
            NavmInstruction popn = new NavmInstruction();
            popn.setParam(1);
            popn.setHasParam(true);
            popn.setOpcode(popn.getOpcode(Instructions.popn));
            body.add(popn);
        }




    }

    public void printInstructions() {
        int num = 0;
        for (NavmInstruction navmInstruction : body) {
            System.out.println("    " + num + " : " + navmInstruction);
            num ++;
        }
    }
}

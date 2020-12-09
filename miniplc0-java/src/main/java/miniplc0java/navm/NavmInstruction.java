package miniplc0java.navm;

import java.util.HashMap;

public class NavmInstruction {
    private int opcode;
    private int param;
    private boolean hasParam;
    private HashMap<Instructions, Integer> instructionMap;

    private HashMap<Integer, Instructions> forOutput;
    public NavmInstruction() {
        this.instructionMap = new HashMap<>();
        this.forOutput = new HashMap<>();
        instructionMap.put(Instructions.push, 0x01);
        instructionMap.put(Instructions.pop, 0x02);
        instructionMap.put(Instructions.popn, 0x03);
        instructionMap.put(Instructions.loca, 0x0a);
        instructionMap.put(Instructions.arga, 0x0b);
        instructionMap.put(Instructions.globa, 0x0c);
        instructionMap.put(Instructions.stackalloc, 0x1a);
        instructionMap.put(Instructions.load, 0x13);
        instructionMap.put(Instructions.store, 0x17);
        instructionMap.put(Instructions.add, 0x20);
        instructionMap.put(Instructions.sub, 0x21);
        instructionMap.put(Instructions.mul, 0x22);
        instructionMap.put(Instructions.div, 0x23);
        instructionMap.put(Instructions.call, 0x48);
        instructionMap.put(Instructions.ret, 0x49);
        instructionMap.put(Instructions.callname, 0x4a);
        instructionMap.put(Instructions.neg, 0x34);
        instructionMap.put(Instructions.cmpi, 0x30);
        instructionMap.put(Instructions.SetLt, 0x39);
        instructionMap.put(Instructions.SetGt, 0x3a);
        instructionMap.put(Instructions.not, 0x2e);
        instructionMap.put(Instructions.BrTrue, 0x43);
        instructionMap.put(Instructions.Br, 0x41);


        forOutput.put(0x01, Instructions.push);
        forOutput.put(0x02, Instructions.pop);
        forOutput.put(0x03, Instructions.popn);
        forOutput.put(0x0a, Instructions.loca);
        forOutput.put(0x0b, Instructions.arga);
        forOutput.put(0x0c, Instructions.globa);
        forOutput.put(0x1a, Instructions.stackalloc);
        forOutput.put(0x13, Instructions.load);
        forOutput.put(0x17, Instructions.store);
        forOutput.put(0x20, Instructions.add);
        forOutput.put(0x21, Instructions.sub);
        forOutput.put(0x22, Instructions.mul);
        forOutput.put(0x23, Instructions.div);
        forOutput.put(0x34, Instructions.neg);
        forOutput.put(0x48, Instructions.call);
        forOutput.put(0x49, Instructions.ret);
        forOutput.put(0x4a, Instructions.callname);
        forOutput.put(0x30, Instructions.cmpi);
        forOutput.put(0x39, Instructions.SetLt);
        forOutput.put(0x3a, Instructions.SetGt);
        forOutput.put(0x2e, Instructions.not);
        forOutput.put(0x43, Instructions.BrTrue);
        forOutput.put(0x41, Instructions.Br);

    }

    public int getOpcode(Instructions instructions) {
        return this.instructionMap.get(instructions);
    }

    public int getOpcode() {
        return this.opcode;
    }
    public void setOpcode(int opcode) {
        this.opcode = opcode;
    }

    public int getParam() {
        return param;
    }

    public void setParam(int param) {
        this.param = param;
    }

    public boolean isHasParam() {
        return hasParam;
    }

    public void setHasParam(boolean hasParam) {
        this.hasParam = hasParam;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        if (this.hasParam) {
            sb.append(forOutput.get(this.opcode));
            sb.append(" ");
            sb.append(this.param);
        } else {
            sb.append(forOutput.get(this.opcode));
        }
        return sb.toString();
    }

    public Instructions getInstruction(int opcode) {
        return forOutput.get(opcode);
    }
}

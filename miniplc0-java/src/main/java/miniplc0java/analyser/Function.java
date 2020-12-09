package miniplc0java.analyser;

import java.util.ArrayList;

public class Function {
    private String functionName;
    private int paramNum;
    private ArrayList<String> paramType;
    private String returnType;

    public Function(String functionName, int paramNum, ArrayList<String> paramType, String returnType) {
        this.functionName = functionName;
        this.paramNum = paramNum;
        this.paramType = paramType;
        this.returnType = returnType;
    }

    public String getFunctionName() {
        return this.functionName;
    }

    public boolean isLegalFunctionCall(int numOfParam) {
        return false;
    }

    public int getParamNum (){
        return this.paramNum;
    }

    public String getReturnType() {
        return this.returnType;
    }
}

package miniplc0java.analyser;

import miniplc0java.error.AnalyzeError;
import miniplc0java.error.ErrorCode;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;

public class FunctionTable {
    private ArrayList<Function> functionList;

    public FunctionTable() {
        this.functionList = new ArrayList<>();
    }

    public void addNewFunction(Function function) {
        functionList.add(function);
    }

    public boolean hasFunction(String functionName) {
        for (Function function : functionList) {
            if (function.getFunctionName().equals(functionName)) {
                return true;
            }
        }
        return false;
    }

    public int getNumOfFunction (String functionName) {
        for (Function function : functionList) {
            if (function.getFunctionName().equals(functionName)) {
                return function.getParamNum();
            }
        }
        return -1;
    }

    public String getReturnTypeOfFunction (String functionName) {
        for (Function function : functionList) {
            if (function.getFunctionName().equals(functionName)) {
                return function.getReturnType();
            }
        }
        return null;
    }
}

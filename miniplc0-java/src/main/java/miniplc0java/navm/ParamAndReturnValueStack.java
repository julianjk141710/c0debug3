package miniplc0java.navm;

import miniplc0java.analyser.SymbolEntry;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

public class ParamAndReturnValueStack {
    int paramAndReturnValueStackPointer = 1;
    HashMap<String, Integer> paramAndReturnValueStack;

    public ParamAndReturnValueStack() {

        this.paramAndReturnValueStack = new HashMap<>();
    }

    public int getParamReturnStackPointer() {
        return paramAndReturnValueStackPointer;
    }

    public void addParamReturnStackPointer() {
        this.paramAndReturnValueStackPointer ++;
    }

    public void addToParamReturnStack(String symbolName, int offset) {
        this.paramAndReturnValueStack.put(symbolName, offset);
    }

    public int getOffset(String paramName) {
        if (paramAndReturnValueStack.get(paramName) != null) {
            return paramAndReturnValueStack.get(paramName);
        }
        return -1;
    }
    public void resetParamAndReturnValueStack() {
        this.paramAndReturnValueStackPointer = 1;
        for (Iterator<Map.Entry<String, Integer>> it = this.paramAndReturnValueStack.entrySet().iterator(); it.hasNext();){
            Map.Entry<String, Integer> item = it.next();
            it.remove();
        }
    }
}

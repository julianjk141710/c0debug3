package miniplc0java.navm;

import miniplc0java.analyser.SymbolEntry;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

public class LocalVariableStack {
    int localVariableStackPointer = 0;
    HashMap<String, Integer> localVariableStack;

    public LocalVariableStack() {
        this.localVariableStack = new HashMap<>();
    }

    public int getLocalStackPointer() {
        return localVariableStackPointer;
    }

    public void addLocalStackPointer() {
        this.localVariableStackPointer ++;
    }

    public void addToLocalStack(String symbolName, int offset) {
        this.localVariableStack.put(symbolName, offset);
    }

    public int getOffset(String symbolName) {
       if (localVariableStack.get(symbolName) != null) {
           return localVariableStack.get(symbolName);
       }
       return -1;
    }

    public void resetLocalVariableStack() {
        this.localVariableStackPointer = 0;
        for (Iterator<Map.Entry<String, Integer>> it = this.localVariableStack.entrySet().iterator(); it.hasNext();){
            Map.Entry<String, Integer> item = it.next();
            it.remove();
        }
    }
}

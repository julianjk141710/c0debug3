package miniplc0java.analyser;

import java.util.HashMap;

public class GlobalStack {
    int globalStackPointer = 0;
    HashMap<String, Integer> globalStack;

    public GlobalStack() {
        this.globalStack = new HashMap<>();
    }

    public int getGlobalStackPointer() {
        return globalStackPointer;
    }

    public void addGlobalStackPointer() {
        this.globalStackPointer ++;
    }

    public void addToGlobalStack(String symbolName, int offset) {
        this.globalStack.put(symbolName, offset);
    }

    public int getGlobalStackOffset(String symbolName) {
        if (this.globalStack.get(symbolName) != null) {
            return this.globalStack.get(symbolName);
        }
        return -1;
    }
}

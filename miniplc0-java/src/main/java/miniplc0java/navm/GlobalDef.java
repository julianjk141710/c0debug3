package miniplc0java.navm;

import miniplc0java.analyser.GlobalSymbolTable;

import java.util.ArrayList;

public class GlobalDef {
    private int is_const;
    private ArrayList<Integer> value;

    private GlobalSymbolTable globalSymbolTable;

    public GlobalDef() {
        this.globalSymbolTable = new GlobalSymbolTable(0);
    }

    public int getIs_const() {
        return is_const;
    }

    public void setIs_const(int is_const) {
        this.is_const = is_const;
    }

    public ArrayList<Integer> getValue() {
        return value;
    }

    public void setValue(ArrayList<Integer> value) {
        this.value = value;
    }

    public GlobalSymbolTable getGlobalSymbolTable() {
        return globalSymbolTable;
    }

    public void setGlobalSymbolTable(GlobalSymbolTable globalSymbolTable) {
        this.globalSymbolTable = globalSymbolTable;
    }
}

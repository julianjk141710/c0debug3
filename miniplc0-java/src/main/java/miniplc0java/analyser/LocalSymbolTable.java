package miniplc0java.analyser;

import miniplc0java.error.AnalyzeError;
import miniplc0java.error.ErrorCode;
//import miniplc0java.navm.ParamReturnStack;
import miniplc0java.tokenizer.Token;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

public class LocalSymbolTable {
    private int depth;
    private HashMap<String, SymbolEntry> localVariables;
    private LocalSymbolTable previousTable;
    private String functionReturnType;
    //private ParamReturnStack paramReturnStack;

    public LocalSymbolTable() {

    }

    public LocalSymbolTable(int depth) {
        this.depth = depth;
        localVariables = new HashMap<>();
        previousTable = null;
        //this.paramReturnStack = new ParamReturnStack();
    }

    /**
     * 向符号表中添加局部变量
     * 如果符号表中已经有该局部变量则抛出异常
     * @param token
     * @param symbolEntry
     * @throws AnalyzeError
     */
    public void addLocalVariable(Token token, SymbolEntry symbolEntry) throws AnalyzeError {
        if (this.localVariables.get(String.valueOf(token.getValue())) != null) {
            throw new AnalyzeError(ErrorCode.DuplicateDeclaration, token.getStartPos());
        } else {
            this.localVariables.put(String.valueOf(token.getValue()), symbolEntry);
        }
    }

    /**
     * 如果传进来的 token 是重复的 就返回 true
     * 否则返回 false
     * @param tokenName
     * @return
     */
    public boolean duplicateSymbol(String tokenName){
        if (this.localVariables.get(tokenName) != null) {
            return true;
        }
        return false;
    }

    /**
     * 判断符号表中是否包含变量名为 symbolName 的变量
     * @param symbolName
     * @return
     */
    public boolean hasSymbol(String symbolName) {
        if (this.localVariables.get(symbolName) != null) {
            return true;
        }

        return false;
    }
    public int getDepth() {
        return depth;
    }

    public void setDepth(int depth) {
        this.depth = depth;
    }

    public HashMap<String, SymbolEntry> getLocalVariables() {
        return localVariables;
    }

    public void setLocalVariables(HashMap<String, SymbolEntry> localVariables) {
        this.localVariables = localVariables;
    }

    public LocalSymbolTable getPreviousTable() {
        return previousTable;
    }

    public void setPreviousTable(LocalSymbolTable previousTable) {
        this.previousTable = previousTable;
    }

    public void setFunctionReturnType(String functionReturnType) {
        this.functionReturnType = functionReturnType;
    }

    public String getFunctionReturnType() {
        return this.functionReturnType;
    }

    public SymbolEntry getSymbolEntry(String tokenName) {
        if (this.localVariables.get(tokenName) != null) {
            return this.localVariables.get(tokenName);
        }
        return null;
    }

    public int getNumOflocalVariables() {
        return this.localVariables.size();
    }

    public int getReturnTypeOfFunction() {
        if (this.functionReturnType.equals("int")) {
            return 1;
        }
        return 0;
    }

    public void resetLocalSymbolTable() {
        this.functionReturnType = null;
        this.depth = 1;
        for (Iterator<Map.Entry<String, SymbolEntry>> it = this.localVariables.entrySet().iterator(); it.hasNext();){
            Map.Entry<String, SymbolEntry> item = it.next();
            it.remove();
        }
        this.previousTable = null;
    }

    public int getSizeOfMap() {
        return this.localVariables.size();
    }
}

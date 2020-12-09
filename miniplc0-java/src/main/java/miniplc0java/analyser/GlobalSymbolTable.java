package miniplc0java.analyser;

import miniplc0java.error.AnalyzeError;
import miniplc0java.error.ErrorCode;
import miniplc0java.tokenizer.Token;

import java.util.HashMap;

public class GlobalSymbolTable extends LocalSymbolTable{
    private int depth;
    private HashMap<String, SymbolEntry> globalVariables;
    private LocalSymbolTable previousTable;
//    private HashMap<String, SymbolEntry> globalVariables;

    public GlobalSymbolTable(int depth) {
        super();
        this.depth = depth;
        this.previousTable = null;
        this.globalVariables = new HashMap<>();
    }

    /**
     * 向符号表中添加局部变量
     * 如果符号表中已经有该局部变量则抛出异常
     * @param token
     * @param symbolEntry
     * @throws AnalyzeError
     */

    public void addGlobalVariable(Token token, SymbolEntry symbolEntry) throws AnalyzeError {
//        if (this.globalVariables.get(String.valueOf(token.getValue())) != null) {
//            throw new AnalyzeError(ErrorCode.DuplicateDeclaration, token.getStartPos());
//        }
        if (hasSymbol(String.valueOf(token.getValue()))) {
            throw new AnalyzeError(ErrorCode.DuplicateDeclaration, token.getStartPos());
        }
        else {
            this.globalVariables.put(String.valueOf(token.getValue()), symbolEntry);
        }
    }


    public boolean hasSymbol(String symbolName) {
        if (globalVariables.get(symbolName) != null) {
            return true;
        }
        return false;
    }

    public boolean duplicateSymbol(String symbolName) {
        if (globalVariables.get(symbolName) != null) {
            return true;
        }
        return false;
    }

    public SymbolEntry getSymbolEntry(String tokenName) {
        if (this.globalVariables.get(tokenName) != null) {
            return this.globalVariables.get(tokenName);
        }
        return null;
    }
}

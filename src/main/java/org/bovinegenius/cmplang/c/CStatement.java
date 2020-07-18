package org.bovinegenius.cmplang.c;

import lombok.NonNull;
import lombok.Value;
import org.bovinegenius.cmplang.ast.module.Ident;
import org.bovinegenius.cmplang.ast.module.Span;

import java.util.List;
import java.util.Objects;

public abstract class CStatement {
    private CStatement() {}

    public String formatted(int indentAmount) {
        return this.formatted(indentAmount, 0);
    }

    protected abstract String formatted(int indentAmount, int indentLevel);

    @Value(staticConstructor = "of")
    public static class Expr extends CStatement {
        CExpr expr;

        @Override
        public String toString() {
            return this.formatted(4);
        }

        @Override
        public String formatted(int indentAmount, int indentLevel) {
            return indent(indentAmount,  indentLevel) + Objects.toString(expr, null) + ";";
        }
    }

    @Value(staticConstructor = "of")
    public static class If extends CStatement {
        CExpr condition;
        CStatement then;
        CStatement elseClause;

        @Override
        public String toString() {
            return formatted(4);
        }

        @Override
        public String formatted(int indentAmount, int indentLevel) {
            StringBuilder sb = new StringBuilder();
            sb.append(indent(indentAmount, indentLevel)).append("if (").append(condition).append(")");
            sb.append(then.formatted(indentAmount, indentLevel));
            sb.append(indent(indentAmount, indentLevel)).append(" else ");
            sb.append(elseClause.formatted(indentAmount, indentLevel));
            return sb.toString();
        }
    }

    @Value(staticConstructor = "of")
    public static class For extends CStatement {
        CExpr init;
        CExpr cond;
        CExpr step;
        CStatement body;

        @Override
        public String toString() {
            return formatted(4);
        }

        @Override
        public String formatted(int indentAmount, int indentLevel) {
            StringBuilder sb = new StringBuilder();
            sb.append(indent(indentAmount, indentLevel)).append("for (").append(init).append(", ").append(cond).append(", ").append(step).append(")");
            sb.append(trimFront(body.formatted(indentAmount, indentLevel)));
            return sb.toString();
        }
    }

    @Value(staticConstructor = "of")
    public static class Return extends CStatement {
        CExpr expr;

        @Override
        public String toString() {
            return formatted(4);
        }

        @Override
        public String formatted(int indentAmount, int indentLevel) {
            StringBuilder sb = new StringBuilder();
            sb.append(indent(indentAmount, indentLevel)).append("return ").append(expr).append(";");
            return sb.toString();
        }
    }

    @Value(staticConstructor = "of")
    public static class Block extends CStatement {
        List<VariableDeclaration> declarations;
        List<CStatement> statements;

        @Override
        public String toString() {
            return formatted(4);
        }

        @Override
        public String formatted(int indentAmount, int indentLevel) {
            StringBuilder sb = new StringBuilder();
            sb.append(indent(indentAmount, indentLevel)).append("{\n");
            for (VariableDeclaration varDecl : declarations) {
                sb.append(varDecl.formatted(indentAmount, indentLevel + 1)).append("\n");
            }
            for (CStatement statement : statements) {
                sb.append(statement.formatted(indentAmount, indentLevel + 1));
            }
            sb.append(indent(indentAmount, indentLevel)).append("}");
            return sb.toString();
        }
    }

    @Value(staticConstructor = "of")
    public static class VariableDeclaration {
        @NonNull Ident<String, Span> name;
        @NonNull CType type;

        public String formatted(int indentAmount, int indentLevel) {
            return indent(indentAmount, indentLevel) + type + " " + name + ";";
        }
    }

    private static String indent(int indentAmount, int indentLevel) {
        final int spaces = indentAmount * indentLevel;
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < spaces; i++) {
            sb.append(" ");
        }
        return sb.toString();
    }

    private static String trimFront(String str) {
        return str.replaceAll("^\\s+", "");
    }

}

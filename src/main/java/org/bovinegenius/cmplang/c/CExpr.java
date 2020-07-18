package org.bovinegenius.cmplang.c;

import lombok.Value;
import org.bovinegenius.cmplang.ast.module.Ident;
import org.bovinegenius.cmplang.ast.module.Path;
import org.bovinegenius.cmplang.ast.module.Span;

import java.util.List;
import java.util.Objects;

public abstract class CExpr {
    private CExpr() {}

    public abstract boolean isComplex();

    @Value(staticConstructor = "of")
    public static class Intconst extends CExpr {
        int value;

        @Override
        public boolean isComplex() {
            return false;
        }

        @Override
        public String toString() {
            return Objects.toString(this.value, null);
        }
    }

    @Value(staticConstructor = "of")
    public static class Floatconst extends CExpr {
        float value;

        @Override
        public boolean isComplex() {
            return false;
        }

        @Override
        public String toString() {
            return Objects.toString(this.value, null);
        }
    }

    @Value(staticConstructor = "of")
    public static class Variable extends CExpr {
        Path<Span, String, Ident<String, Span>> value;

        @Override
        public boolean isComplex() {
            return false;
        }

        @Override
        public String toString() {
            return Objects.toString(this.value, null);
        }
    }

    @Value(staticConstructor = "of")
    public static class Apply extends CExpr {
        CExpr func;
        List<CExpr> parameters;

        @Override
        public boolean isComplex() {
            return false;
        }

        @Override
        public String toString() {
            StringBuilder sb = new StringBuilder();
            if (func.isComplex()) {
                sb.append("(").append(func).append(")");
            } else {
                sb.append(func);
            }
            sb.append("(");
            boolean first = true;
            for (CExpr param : parameters) {
                if (first) {
                    first = false;
                } else {
                    sb.append(", ");
                }
                sb.append(param);
            }
            sb.append(")");
            return sb.toString();
        }
    }

    @Value(staticConstructor = "of")
    public static class Assign extends CExpr {
        CExpr var;
        CExpr value;

        @Override
        public boolean isComplex() {
            return true;
        }

        @Override
        public String toString() {
            return String.format("%s = %s", this.var, this.value);
        }
    }

    @Value(staticConstructor = "of")
    public static class UnaryOp extends CExpr {
        String operator;
        CExpr expr;

        @Override
        public boolean isComplex() {
            return true;
        }

        @Override
        public String toString() {
            StringBuilder sb = new StringBuilder();
            sb.append(this.operator);
            if (expr.isComplex()) {
                sb.append("(").append(expr).append(")");
            } else {
                sb.append(expr);
            }
            return sb.toString();
        }
    }

    @Value(staticConstructor = "of")
    public static class BinaryOp extends CExpr {
        String operator;
        CExpr left;
        CExpr right;

        @Override
        public boolean isComplex() {
            return true;
        }

        @Override
        public String toString() {
            StringBuilder sb = new StringBuilder();
            if (left.isComplex()) {
                sb.append("(").append(left).append(")");
            } else {
                sb.append(left);
            }
            sb.append(" ").append(operator).append(" ");
            if (right.isComplex()) {
                sb.append("(").append(right).append(")");
            } else {
                sb.append(right);
            }
            return sb.toString();
        }
    }

    @Value(staticConstructor = "of")
    public static class Cast extends CExpr {
        CType type;
        CExpr expr;

        @Override
        public boolean isComplex() {
            return true;
        }

        @Override
        public String toString() {
            return String.format("(%s)%s", type, expr);
        }
    }

}

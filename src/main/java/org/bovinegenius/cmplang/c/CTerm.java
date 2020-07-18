package org.bovinegenius.cmplang.c;

import lombok.NonNull;
import lombok.Value;
import org.bovinegenius.cmplang.ast.module.Ident;
import org.bovinegenius.cmplang.ast.module.Span;

import java.util.List;
import java.util.Objects;

public abstract class CTerm {
    private CTerm() {}

    @Value(staticConstructor = "of")
    public static class VarDecl extends CTerm {
        @NonNull CType type;

        @Override
        public String toString() {
            return Objects.toString(type, null);
        }
    }

    @Value(staticConstructor = "of")
    public static class FuncDef extends CTerm {
        @NonNull List<Arg> args;
        @NonNull CType returnType;
        @NonNull CStatement body;

        public String toString() {
            StringBuilder sb = new StringBuilder();
            sb.append(returnType).append(" (");
            boolean first = true;
            for (Arg arg : args) {
                if (first) {
                    first = false;
                } else {
                    sb.append(", ");
                }
                sb.append(arg);
            }
            sb.append(")");
            sb.append(trimFront(body.formatted(4)));
            return sb.toString();
        }
    }

    @Value(staticConstructor = "of")
    public static class Arg {
        @NonNull Ident<String, Span> name;
        @NonNull CType type;

        public String toString() {
            return String.format("%s %s", type, name);
        }
    }

    private static String trimFront(String str) {
        return str.replaceAll("^\\s+", "");
    }

}

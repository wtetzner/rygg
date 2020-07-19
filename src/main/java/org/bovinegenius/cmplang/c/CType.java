package org.bovinegenius.cmplang.c;

import lombok.NonNull;
import lombok.Value;
import org.apache.commons.lang3.NotImplementedException;
import org.bovinegenius.cmplang.ast.module.Path;
import org.bovinegenius.cmplang.ast.module.Span;
import org.bovinegenius.cmplang.ast.module.Subst;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

public abstract class CType {
    private CType() {
    }

    public abstract CType subst(Subst<Span, String> subst);

    public abstract boolean matches(CType other);

    @Value(staticConstructor = "create")
    public static class Void extends CType {
        @Override
        public String toString() {
            return "void";
        }

        @Override
        public CType subst(Subst<Span, String> subst) {
            return this;
        }

        @Override
        public boolean matches(@NonNull CType other) {
            return (other instanceof Void);
        }
    }

    @Value(staticConstructor = "create")
    public static class Int extends CType {
        @Override
        public String toString() {
            return "int";
        }

        @Override
        public CType subst(Subst<Span, String> subst) {
            return this;
        }

        @Override
        public boolean matches(@NonNull CType other) {
            return (other instanceof Int);
        }
    }

    @Value(staticConstructor = "create")
    public static class Float extends CType {
        @Override
        public String toString() {
            return "float";
        }

        @Override
        public CType subst(Subst<Span, String> subst) {
            return this;
        }

        @Override
        public boolean matches(@NonNull CType other) {
            return (other instanceof Float);
        }
    }

    @Value(staticConstructor = "of")
    public static class Pointer extends CType {
        CType pointedType;

        @Override
        public String toString() {
            return "*" + this.pointedType;
        }

        @Override
        public CType subst(Subst<Span, String> subst) {
            return Pointer.of(pointedType.subst(subst));
        }

        @Override
        public boolean matches(@NonNull CType other) {
            if (other instanceof Pointer) {
                return this.getPointedType().matches(((Pointer) other).getPointedType());
            } else {
                return false;
            }
        }
    }

    @Value(staticConstructor = "of")
    public static class Function extends CType {
        List<CType> argTypes;
        CType returnType;

        @Override
        public String toString() {
            StringBuilder sb = new StringBuilder();
            sb.append(returnType).append(" (");
            boolean first = true;
            for (CType ctype : argTypes) {
                if (first) {
                    first = false;
                } else {
                    sb.append(", ");
                }
                sb.append(ctype);
            }
            sb.append(")");
            return sb.toString();
        }

        @Override
        public CType subst(Subst<Span, String> subst) {
            List<CType> substArgTypes = argTypes.stream().map(t -> t.subst(subst)).collect(Collectors.toList());
            return Function.of(substArgTypes, returnType.subst(subst));
        }

        @Override
        public boolean matches(@NonNull CType other) {
            if (true) {
                // TODO: Finish implementing this
                throw new NotImplementedException("matches");
            }
            if (other instanceof Function) {
                Function otherFunc = (Function) other;
                if (this.getArgTypes().size() == otherFunc.getArgTypes().size()) {
                    for (int i = 0; i < this.getArgTypes().size(); i++) {

                    }
                } else {
                    return false;
                }
            } else {

                return false;
            }
            throw new NotImplementedException("matches");
        }
    }

    @Value(staticConstructor = "of")
    public static class TypeName extends CType {
        Path<Span, String> path;

        @Override
        public String toString() {
            return Objects.toString(this.path, null);
        }

        @Override
        public CType subst(Subst<Span, String> subst) {
            return TypeName.of(subst.apply(path));
        }

        @Override
        public boolean matches(@NonNull CType other) {
            // TODO: Implement this
            throw new NotImplementedException("matches");
        }
    }

}

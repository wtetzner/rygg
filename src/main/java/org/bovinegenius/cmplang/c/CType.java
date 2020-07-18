package org.bovinegenius.cmplang.c;

import lombok.NonNull;
import lombok.Value;
import org.bovinegenius.cmplang.ast.module.*;
import org.bovinegenius.cmplang.ast.module.error.TypeError;
import org.bovinegenius.cmplang.ast.module.error.UndelcaredType;
import org.bovinegenius.cmplang.util.Result;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

public abstract class CType {
    private CType() {}

    public abstract CType subst(Subst<Span, String, Ident<String, Span>> subst);
    public abstract Result<Optional<CType>, TypeError<Span>> expand(Env<Span, String, Ident<String, Span>, CTerm, CType, CType, java.lang.Void> env);
    public abstract boolean matches(CType other);

    @Value(staticConstructor = "create")
    public static class Void extends CType {
        @Override
        public String toString() {
            return "void";
        }

        @Override
        public CType subst(Subst<Span, String, Ident<String, Span>> subst) {
            return this;
        }

        @Override
        public boolean matches(@NonNull CType other) {
            return (other instanceof Void);
        }

        @Override
        public Result<Optional<CType>, TypeError<Span>> expand(Env<Span, String, Ident<String, Span>, CTerm, CType, CType, java.lang.Void> env) {
            return Result.ok(this);
        }
    }

    @Value(staticConstructor = "create")
    public static class Int extends CType {
        @Override
        public String toString() {
            return "int";
        }

        @Override
        public CType subst(Subst<Span, String, Ident<String, Span>> subst) {
            return this;
        }

        @Override
        public boolean matches(@NonNull CType other) {
            return (other instanceof Int);
        }

        @Override
        public Result<Optional<CType>, TypeError<Span>> expand(Env<Span, String, Ident<String, Span>, CTerm, CType, CType, java.lang.Void> env) {
            return Result.ok(this);
        }
    }

    @Value(staticConstructor = "create")
    public static class Float extends CType {
        @Override
        public String toString() {
            return "float";
        }

        @Override
        public CType subst(Subst<Span, String, Ident<String, Span>> subst) {
            return this;
        }

        @Override
        public boolean matches(@NonNull CType other) {
            return (other instanceof Float);
        }

        @Override
        public Result<Optional<CType>, TypeError<Span>> expand(Env<Span, String, Ident<String, Span>, CTerm, CType, CType, java.lang.Void> env) {
            return Result.ok(this);
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
        public CType subst(Subst<Span, String, Ident<String, Span>> subst) {
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

        @Override
        public Result<Optional<CType>, TypeError<Span>> expand(Env<Span, String, Ident<String, Span>, CTerm, CType, CType, java.lang.Void> env) {
            return this.getPointedType().expand(env).map(Pointer::of);
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
        public CType subst(Subst<Span, String, Ident<String, Span>> subst) {
            List<CType> substArgTypes = argTypes.stream().map(t -> t.subst(subst)).collect(Collectors.toList());
            return Function.of(substArgTypes, returnType.subst(subst));
        }

        @Override
        public boolean matches(@NonNull CType other) {
            if (other instanceof Function) {
                Function otherFunc = (Function)other;
                if (this.getArgTypes().size() == otherFunc.getArgTypes().size()) {
                    for (int i = 0; i < this.getArgTypes().size(); i++) {

                    }
                } else {
                    return false;
                }
            } else {
                return false;
            }
        }

        @Override
        public Result<Optional<CType>, TypeError<Span>> expand(Env<Span, String, Ident<String, Span>, CTerm, CType, CType, java.lang.Void> env) {
            List<CType> expandedArgTypes = new ArrayList<>();
            for (CType type : this.getArgTypes()) {
                Result<Optional<CType>, TypeError<Span>> result = type.expand(env);
                if (result.isErr() || !result.get().isPresent()) {
                    return result.mapErr(java.util.function.Function.identity());
                }
                expandedArgTypes.add(result.get().get());
            }
            Result<Optional<CType>, TypeError<Span>> result = returnType.expand(env);
            if (result.isErr() || !result.get().isPresent()) {
                return result.mapErr(java.util.function.Function.identity());
            }
            return Result.ok(Function.of(expandedArgTypes, result.get().get()));
        }
    }

    @Value(staticConstructor = "of")
    public static class TypeName extends CType {
        Path<Span, String, Ident<String, Span>> path;

        @Override
        public String toString() {
            return Objects.toString(this.path, null);
        }

        @Override
        public CType subst(Subst<Span, String, Ident<String, Span>> subst) {
            return TypeName.of(subst.apply(path));
        }

        @Override
        public Result<Optional<CType>, TypeError<Span>> expand(Env<Span, String, Ident<String, Span>, CTerm, CType, CType, java.lang.Void> env) {
            Optional<TypeDecl<java.lang.Void, CType>> foundType = env.findType(this.path);
            if (!foundType.isPresent()) {
                return Result.err(UndelcaredType.<Span, CType>builder()
                        .location(this.path.getIdent().getLocation())
                        .type(this)
                .build());
            }

        }
    }

}

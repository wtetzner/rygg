package org.bovinegenius.cmplang.c;

import org.bovinegenius.cmplang.ast.module.*;
import org.bovinegenius.cmplang.ast.module.error.TypeError;
import org.bovinegenius.cmplang.util.Result;

import org.bovinegenius.cmplang.ast.module.error.UndelcaredType;

import java.lang.reflect.UndeclaredThrowableException;
import java.util.Optional;
import java.util.function.Function;

public class CTyping implements CoreTyping<Span, String, CTerm, CType, CType, Void> {
    @Override
    public Result<CType, TypeError<Span>> typeTerm(Env<Span, String, CTerm, CType, CType, Void> env, CTerm cTerm) {
        return null;
    }

    @Override
    public Result<Void, TypeError<Span>> kindDeftype(Env<Span, String, CTerm, CType, CType, Void> env, CType cType) {
        return checkValtype(env, cType);
    }

    @Override
    public Result<Void, TypeError<Span>> checkValtype(Env<Span, String, CTerm, CType, CType, Void> env, CType cType) {
        if (cType instanceof CType.TypeName) {
            CType.TypeName typename = (CType.TypeName)cType;
            Optional<TypeDecl<Void, CType>> foundType = env.findType(typename.getPath());
            if (!foundType.isPresent()) {
                return Result.err(UndelcaredType.<Span, CType>builder()
                        .location(((CType.TypeName) cType).getPath().getIdent().getLocation())
                        .type(cType)
                        .build());
            }
            return Result.ok();
        } else if (cType instanceof CType.Pointer) {
            CType.Pointer pointer = (CType.Pointer)cType;
            return checkValtype(env, pointer.getPointedType());
        } else if (cType instanceof CType.Function) {
            CType.Function function = (CType.Function)cType;
            for (CType argType : function.getArgTypes()) {
                Result<Void, TypeError<Span>> result = checkValtype(env, argType);
                if (result.isErr()) {
                    return result.mapErr(Function.identity());
                }
            }
            return checkValtype(env, function.getReturnType());
        }
        return Result.ok();
    }

    @Override
    public Result<Void, TypeError<Span>> checkKind(Env<Span, String, CTerm, CType, CType, Void> env, Void aVoid) {
        return Result.ok();
    }

    @Override
    public boolean valtypeMatch(Env<Span, String, CTerm, CType, CType, Void> env, CType val1, CType val2) {
        return false;
    }

    @Override
    public boolean deftypeEquiv(Env<Span, String, CTerm, CType, CType, Void> env, Void aVoid, CType def1, CType def2) {
        return false;
    }

    @Override
    public boolean kindMatch(Env<Span, String, CTerm, CType, CType, Void> env, Void kind1, Void kind2) {
        return false;
    }

    @Override
    public CType deftypeOfPath(Path<Span, String> path, Void aVoid) {
        return CType.TypeName.of(path);
    }
}

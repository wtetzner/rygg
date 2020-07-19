package org.bovinegenius.cmplang.c;

import org.bovinegenius.cmplang.ast.module.CoreSyntax;
import org.bovinegenius.cmplang.ast.module.Span;
import org.bovinegenius.cmplang.ast.module.Subst;

public class C implements CoreSyntax<Span, String, CTerm, CType, CType, Void> {

    @Override
    public CType substVal(CType cType, Subst<Span, String> subst) {
        return cType.subst(subst);
    }

    @Override
    public CType substDef(CType cType, Subst<Span, String> subst) {
        return cType.subst(subst);
    }

    @Override
    public Void substKind(Void aVoid, Subst<Span, String> subst) {
        return aVoid;
    }

}

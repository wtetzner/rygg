package org.bovinegenius.cmplang.ast.module;

public interface CoreSyntax<LOC, NAME, IDENT extends Ident<NAME,LOC>, TERM, VAL, DEF, KIND> {
    public VAL substVal(VAL val, Subst<LOC, NAME, IDENT> subst);
    public DEF substDef(DEF def, Subst<LOC, NAME, IDENT> subst);
    public KIND substKind(KIND kind, Subst<LOC, NAME, IDENT> subst);
}

package org.bovinegenius.cmplang;

public interface CoreSyntax<NAME, IDENT extends Ident<NAME>, TERM, VAL, DEF, KIND> {
    public VAL substVal(VAL val, Subst<NAME, IDENT> subst);
    public DEF substDef(DEF def, Subst<NAME, IDENT> subst);
    public KIND substKind(KIND kind, Subst<NAME, IDENT> subst);
}

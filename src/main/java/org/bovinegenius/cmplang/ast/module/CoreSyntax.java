package org.bovinegenius.cmplang.ast.module;

public interface CoreSyntax<LOC, NAME, TERM, VAL, DEF, KIND> {
    public VAL substVal(VAL val, Subst<LOC, NAME> subst);
    public DEF substDef(DEF def, Subst<LOC, NAME> subst);
    public KIND substKind(KIND kind, Subst<LOC, NAME> subst);
}

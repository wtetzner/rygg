package org.bovinegenius.cmplang;

public interface CoreTyping<NAME, IDENT extends Ident<NAME>, TERM, VAL, DEF, KIND> {

    public VAL typeTerm(Env<NAME, IDENT, TERM, VAL, DEF, KIND> env, TERM term);
    public KIND kindDeftype(Env<NAME, IDENT, TERM, VAL, DEF, KIND> env, DEF def);
    public void checkValtype(Env<NAME, IDENT, TERM, VAL, DEF, KIND> env, VAL val);
    public void checkKind(Env<NAME, IDENT, TERM, VAL, DEF, KIND> env, KIND kind);
    
    public boolean valtypeMatch(Env<NAME, IDENT, TERM, VAL, DEF, KIND> env, VAL val1, VAL val2);
    public boolean deftypeEquiv(Env<NAME, IDENT, TERM, VAL, DEF, KIND> env, KIND kind, DEF def1, DEF def2);
    public boolean kindMatch(Env<NAME, IDENT, TERM, VAL, DEF, KIND> env, KIND kind1, KIND kind2);
    public DEF deftypeOfPath(Path<NAME, IDENT> path, KIND kind, DEF deftype);
}

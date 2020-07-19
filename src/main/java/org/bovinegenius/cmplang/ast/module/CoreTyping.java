package org.bovinegenius.cmplang.ast.module;

import org.bovinegenius.cmplang.ast.module.error.TypeError;
import org.bovinegenius.cmplang.util.Result;

public interface CoreTyping<LOC, NAME, TERM, VAL, DEF, KIND> {

    public Result<VAL, TypeError<LOC>> typeTerm(Env<LOC, NAME, TERM, VAL, DEF, KIND> env, TERM term);
    public Result<KIND, TypeError<LOC>> kindDeftype(Env<LOC, NAME, TERM, VAL, DEF, KIND> env, DEF def);
    public Result<Void, TypeError<LOC>> checkValtype(Env<LOC, NAME, TERM, VAL, DEF, KIND> env, VAL val);
    public Result<Void, TypeError<LOC>> checkKind(Env<LOC, NAME, TERM, VAL, DEF, KIND> env, KIND kind);
    
    public boolean valtypeMatch(Env<LOC, NAME, TERM, VAL, DEF, KIND> env, VAL val1, VAL val2);
    public boolean deftypeEquiv(Env<LOC, NAME, TERM, VAL, DEF, KIND> env, KIND kind, DEF def1, DEF def2);
    public boolean kindMatch(Env<LOC, NAME, TERM, VAL, DEF, KIND> env, KIND kind1, KIND kind2);
    public DEF deftypeOfPath(Path<LOC, NAME> path, KIND kind);
}

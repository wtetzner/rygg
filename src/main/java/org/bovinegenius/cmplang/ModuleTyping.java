package org.bovinegenius.cmplang;

public class ModuleTyping<NAME, IDENT extends Ident<NAME>, TERM, VAL, DEF, KIND> {
    private final ModuleSyntax<NAME, IDENT, TERM, VAL, DEF, KIND> moduleSyntax;
    private final CoreTyping<NAME, IDENT, TERM, VAL, DEF, KIND> coreTyping;

    private ModuleTyping(ModuleSyntax<NAME, IDENT, TERM, VAL, DEF, KIND> moduleSyntax, CoreTyping<NAME, IDENT, TERM, VAL, DEF, KIND> coreTyping) {
        this.moduleSyntax = moduleSyntax;
        this.coreTyping = coreTyping;
    }

    public ModuleType<NAME, IDENT, VAL, KIND, DEF> typeModule(Env<NAME, IDENT, TERM, VAL, DEF, KIND> env, ModuleTerm<NAME, IDENT, VAL, KIND, DEF, TERM> modTerm) {
        
    }

    public Specification<NAME, IDENT, VAL, KIND, DEF> typeDefinition(Env<NAME, IDENT, TERM, VAL, DEF, KIND> env, Definition<NAME, IDENT, VAL, KIND, DEF, TERM> definition) {

    }

    
    private void modtypeMatch(Env<NAME, IDENT, TERM, VAL, DEF, KIND> env, ModuleType<NAME, IDENT, VAL, KIND, DEF> modType1, ModuleType<NAME, IDENT, VAL, KIND, DEF> modType2) {

    }

    private ModuleType<NAME, IDENT, VAL, KIND, DEF> strengthenModtype(Path<NAME, IDENT> path, ModuleType<NAME, IDENT, VAL, KIND, DEF> modType) {

    }
}

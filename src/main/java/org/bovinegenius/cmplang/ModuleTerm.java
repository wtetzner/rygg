package org.bovinegenius.cmplang;

import org.pcollections.PVector;

public abstract class ModuleTerm<NAME, IDENT extends Ident<NAME>, VAL, KIND, DEF, TERM> {
    private ModuleTerm() {}

    public static class LongIdent<NAME, IDENT extends Ident<NAME>, VAL, KIND, DEF, TERM> extends ModuleTerm<NAME, IDENT, VAL, KIND, DEF, TERM> {
        private final Path<NAME, IDENT> path;

        private LongIdent(Path<NAME, IDENT> path) {
            this.path = path;
        }

        public Path<NAME, IDENT> getPath() {
            return this.path;
        }
    }

    public static class Structure<NAME, IDENT extends Ident<NAME>, VAL, KIND, DEF, TERM> extends ModuleTerm<NAME, IDENT, VAL, KIND, DEF, TERM> {
        private final PVector<Definition<NAME, IDENT, VAL, KIND, DEF, TERM>> definitions;

        private Structure(PVector<Definition<NAME, IDENT, VAL, KIND, DEF, TERM>> definitions) {
            this.definitions = definitions;
        }

        public PVector<Definition<NAME, IDENT, VAL, KIND, DEF, TERM>> getDefinitions() {
            return this.definitions;
        }
    }

    public static class Functor<NAME, IDENT extends Ident<NAME>, VAL, KIND, DEF, TERM> extends ModuleTerm<NAME, IDENT, VAL, KIND, DEF, TERM> {
        private final IDENT argName;
        private final ModuleType<NAME, IDENT, VAL, KIND, DEF> argType;
        private final ModuleTerm<NAME, IDENT, VAL, KIND, DEF, TERM> body;

        private Functor(IDENT argName, ModuleType<NAME, IDENT, VAL, KIND, DEF> argType, ModuleTerm<NAME, IDENT, VAL, KIND, DEF, TERM> body) {
            this.argName = argName;
            this.argType = argType;
            this.body = body;
        }

        public IDENT getArgName() {
            return this.argName;
        }

        public ModuleType<NAME, IDENT, VAL, KIND, DEF> getArgType() {
            return this.argType;
        }

        public ModuleTerm<NAME, IDENT, VAL, KIND, DEF, TERM> getBody() {
            return this.body;
        }
    }

    public static class Apply<NAME, IDENT extends Ident<NAME>, VAL, KIND, DEF, TERM> extends ModuleTerm<NAME, IDENT, VAL, KIND, DEF, TERM> {
        private final ModuleTerm<NAME, IDENT, VAL, KIND, DEF, TERM> functor;
        private final ModuleTerm<NAME, IDENT, VAL, KIND, DEF, TERM> arg;

        private Apply(ModuleTerm<NAME, IDENT, VAL, KIND, DEF, TERM> functor, ModuleTerm<NAME, IDENT, VAL, KIND, DEF, TERM> arg) {
            this.functor = functor;
            this.arg = arg;
        }

        public ModuleTerm<NAME, IDENT, VAL, KIND, DEF, TERM> getFunctor() {
            return this.functor;
        }

        public ModuleTerm<NAME, IDENT, VAL, KIND, DEF, TERM> getArg() {
            return this.arg;
        }
    }

    public static class Constraint<NAME, IDENT extends Ident<NAME>, VAL, KIND, DEF, TERM> extends ModuleTerm<NAME, IDENT, VAL, KIND, DEF, TERM> {
        private final ModuleTerm<NAME, IDENT, VAL, KIND, DEF, TERM> module;
        private final ModuleType<NAME, IDENT, VAL, KIND, DEF> type;

        private Constraint(ModuleTerm<NAME, IDENT, VAL, KIND, DEF, TERM> module, ModuleType<NAME, IDENT, VAL, KIND, DEF> type) {
            this.module = module;
            this.type = type;
        }

        public ModuleTerm<NAME, IDENT, VAL, KIND, DEF, TERM> getModule() {
            return this.module;
        }

        public ModuleType<NAME, IDENT, VAL, KIND, DEF> getType() {
            return this.type;
        }
    }
}

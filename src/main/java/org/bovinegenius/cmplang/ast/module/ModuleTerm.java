package org.bovinegenius.cmplang.ast.module;

import lombok.Getter;
import org.pcollections.PVector;

public abstract class ModuleTerm<LOC, NAME, IDENT extends Ident<NAME,LOC>, VAL, KIND, DEF, TERM> {
    private ModuleTerm() {}

    public abstract LOC getLocation();

    public static class LongIdent<LOC, NAME, IDENT extends Ident<NAME,LOC>, VAL, KIND, DEF, TERM> extends ModuleTerm<LOC, NAME, IDENT, VAL, KIND, DEF, TERM> {
        @Getter private final LOC location;
        private final Path<LOC, NAME, IDENT> path;

        private LongIdent(LOC location, Path<LOC, NAME, IDENT> path) {
            this.location = location;
            this.path = path;
        }

        public Path<LOC, NAME, IDENT> getPath() {
            return this.path;
        }
    }

    public static class Structure<LOC, NAME, IDENT extends Ident<NAME, LOC>, VAL, KIND, DEF, TERM> extends ModuleTerm<LOC, NAME, IDENT, VAL, KIND, DEF, TERM> {
        @Getter private final LOC location;
        private final PVector<Definition<LOC, NAME, IDENT, VAL, KIND, DEF, TERM>> definitions;

        private Structure(LOC location, PVector<Definition<LOC, NAME, IDENT, VAL, KIND, DEF, TERM>> definitions) {
            this.location = location;
            this.definitions = definitions;
        }

        public PVector<Definition<LOC, NAME, IDENT, VAL, KIND, DEF, TERM>> getDefinitions() {
            return this.definitions;
        }

        public Structure<LOC, NAME, IDENT, VAL, KIND, DEF, TERM> withDefinitions(PVector<Definition<LOC, NAME, IDENT, VAL, KIND, DEF, TERM>> definitions) {
            return new Structure<>(this.location, definitions);
        }
    }

    public static class Functor<LOC, NAME, IDENT extends Ident<NAME, LOC>, VAL, KIND, DEF, TERM> extends ModuleTerm<LOC, NAME, IDENT, VAL, KIND, DEF, TERM> {
        @Getter private final LOC location;
        private final IDENT argName;
        private final ModuleType<LOC, NAME, IDENT, VAL, KIND, DEF> argType;
        private final ModuleTerm<LOC, NAME, IDENT, VAL, KIND, DEF, TERM> body;

        private Functor(LOC location, IDENT argName, ModuleType<LOC, NAME, IDENT, VAL, KIND, DEF> argType, ModuleTerm<LOC, NAME, IDENT, VAL, KIND, DEF, TERM> body) {
            this.location = location;
            this.argName = argName;
            this.argType = argType;
            this.body = body;
        }

        public IDENT getArgName() {
            return this.argName;
        }

        public ModuleType<LOC, NAME, IDENT, VAL, KIND, DEF> getArgType() {
            return this.argType;
        }

        public ModuleTerm<LOC, NAME, IDENT, VAL, KIND, DEF, TERM> getBody() {
            return this.body;
        }
    }

    public static class Apply<LOC, NAME, IDENT extends Ident<NAME, LOC>, VAL, KIND, DEF, TERM> extends ModuleTerm<LOC, NAME, IDENT, VAL, KIND, DEF, TERM> {
        @Getter private final LOC location;
        private final ModuleTerm<LOC, NAME, IDENT, VAL, KIND, DEF, TERM> functor;
        private final ModuleTerm<LOC, NAME, IDENT, VAL, KIND, DEF, TERM> arg;

        private Apply(LOC location, ModuleTerm<LOC, NAME, IDENT, VAL, KIND, DEF, TERM> functor, ModuleTerm<LOC, NAME, IDENT, VAL, KIND, DEF, TERM> arg) {
            this.location = location;
            this.functor = functor;
            this.arg = arg;
        }

        public ModuleTerm<LOC, NAME, IDENT, VAL, KIND, DEF, TERM> getFunctor() {
            return this.functor;
        }

        public ModuleTerm<LOC, NAME, IDENT, VAL, KIND, DEF, TERM> getArg() {
            return this.arg;
        }
    }

    public static class Constraint<LOC, NAME, IDENT extends Ident<NAME, LOC>, VAL, KIND, DEF, TERM> extends ModuleTerm<LOC, NAME, IDENT, VAL, KIND, DEF, TERM> {
        @Getter private final LOC location;
        private final ModuleTerm<LOC, NAME, IDENT, VAL, KIND, DEF, TERM> module;
        private final ModuleType<LOC, NAME, IDENT, VAL, KIND, DEF> type;

        private Constraint(LOC location, ModuleTerm<LOC, NAME, IDENT, VAL, KIND, DEF, TERM> module, ModuleType<LOC, NAME, IDENT, VAL, KIND, DEF> type) {
            this.location = location;
            this.module = module;
            this.type = type;
        }

        public ModuleTerm<LOC, NAME, IDENT, VAL, KIND, DEF, TERM> getModule() {
            return this.module;
        }

        public ModuleType<LOC, NAME, IDENT, VAL, KIND, DEF> getType() {
            return this.type;
        }
    }
}

package org.bovinegenius.cmplang.ast.module;

import lombok.Getter;
import org.pcollections.PVector;

public abstract class ModuleTerm<LOC, NAME, VAL, KIND, DEF, TERM> {
    private ModuleTerm() {}

    public abstract LOC getLocation();

    public static class LongIdent<LOC, NAME, VAL, KIND, DEF, TERM> extends ModuleTerm<LOC, NAME, VAL, KIND, DEF, TERM> {
        @Getter private final LOC location;
        private final Path<LOC, NAME> path;

        private LongIdent(LOC location, Path<LOC, NAME> path) {
            this.location = location;
            this.path = path;
        }

        public Path<LOC, NAME> getPath() {
            return this.path;
        }
    }

    public static class Structure<LOC, NAME, VAL, KIND, DEF, TERM> extends ModuleTerm<LOC, NAME, VAL, KIND, DEF, TERM> {
        @Getter private final LOC location;
        private final PVector<Definition<LOC, NAME, VAL, KIND, DEF, TERM>> definitions;

        private Structure(LOC location, PVector<Definition<LOC, NAME, VAL, KIND, DEF, TERM>> definitions) {
            this.location = location;
            this.definitions = definitions;
        }

        public PVector<Definition<LOC, NAME, VAL, KIND, DEF, TERM>> getDefinitions() {
            return this.definitions;
        }

        public Structure<LOC, NAME, VAL, KIND, DEF, TERM> withDefinitions(PVector<Definition<LOC, NAME, VAL, KIND, DEF, TERM>> definitions) {
            return new Structure<>(this.location, definitions);
        }
    }

    public static class Functor<LOC, NAME, VAL, KIND, DEF, TERM> extends ModuleTerm<LOC, NAME, VAL, KIND, DEF, TERM> {
        @Getter private final LOC location;
        private final Ident<LOC, NAME> argName;
        private final ModuleType<LOC, NAME, VAL, KIND, DEF> argType;
        private final ModuleTerm<LOC, NAME, VAL, KIND, DEF, TERM> body;

        private Functor(LOC location, Ident<LOC, NAME> argName, ModuleType<LOC, NAME, VAL, KIND, DEF> argType, ModuleTerm<LOC, NAME, VAL, KIND, DEF, TERM> body) {
            this.location = location;
            this.argName = argName;
            this.argType = argType;
            this.body = body;
        }

        public Ident<LOC, NAME> getArgName() {
            return this.argName;
        }

        public ModuleType<LOC, NAME, VAL, KIND, DEF> getArgType() {
            return this.argType;
        }

        public ModuleTerm<LOC, NAME, VAL, KIND, DEF, TERM> getBody() {
            return this.body;
        }
    }

    public static class Apply<LOC, NAME, VAL, KIND, DEF, TERM> extends ModuleTerm<LOC, NAME, VAL, KIND, DEF, TERM> {
        @Getter private final LOC location;
        private final ModuleTerm<LOC, NAME, VAL, KIND, DEF, TERM> functor;
        private final ModuleTerm<LOC, NAME, VAL, KIND, DEF, TERM> arg;

        private Apply(LOC location, ModuleTerm<LOC, NAME, VAL, KIND, DEF, TERM> functor, ModuleTerm<LOC, NAME, VAL, KIND, DEF, TERM> arg) {
            this.location = location;
            this.functor = functor;
            this.arg = arg;
        }

        public ModuleTerm<LOC, NAME, VAL, KIND, DEF, TERM> getFunctor() {
            return this.functor;
        }

        public ModuleTerm<LOC, NAME, VAL, KIND, DEF, TERM> getArg() {
            return this.arg;
        }
    }

    public static class Constraint<LOC, NAME, VAL, KIND, DEF, TERM> extends ModuleTerm<LOC, NAME, VAL, KIND, DEF, TERM> {
        @Getter private final LOC location;
        private final ModuleTerm<LOC, NAME, VAL, KIND, DEF, TERM> module;
        private final ModuleType<LOC, NAME, VAL, KIND, DEF> type;

        private Constraint(LOC location, ModuleTerm<LOC, NAME, VAL, KIND, DEF, TERM> module, ModuleType<LOC, NAME, VAL, KIND, DEF> type) {
            this.location = location;
            this.module = module;
            this.type = type;
        }

        public ModuleTerm<LOC, NAME, VAL, KIND, DEF, TERM> getModule() {
            return this.module;
        }

        public ModuleType<LOC, NAME, VAL, KIND, DEF> getType() {
            return this.type;
        }
    }
}

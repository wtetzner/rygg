package org.bovinegenius.cmplang.ast.module;

public abstract class Definition<LOC, NAME, VAL, KIND, DEF, TERM> {
    private Definition() {}

    public static class ValueDef<LOC, NAME, VAL, KIND, DEF, TERM> extends Definition<LOC, NAME, VAL, KIND, DEF, TERM> {
        private final Ident<LOC, NAME> name;
        private final TERM value;

        private ValueDef(Ident<LOC, NAME> name, TERM value) {
            this.name = name;
            this.value = value;
        }

        public Ident<LOC, NAME> getName() {
            return this.name;
        }

        public TERM getValue() {
            return this.value;
        }
    }

    public static class TypeDef<LOC, NAME, VAL, KIND, DEF, TERM> extends Definition<LOC, NAME, VAL, KIND, DEF, TERM> {
        private final Ident<LOC, NAME> name;
        private final KIND kind;
        private final DEF definition;

        private TypeDef(Ident<LOC, NAME> name, KIND kind, DEF definition) {
            this.name = name;
            this.kind = kind;
            this.definition = definition;
        }

        public Ident<LOC, NAME> getName() {
            return this.name;
        }

        public KIND getKind() {
            return this.kind;
        }

        public DEF getDefinition() {
            return this.definition;
        }
    }

    public static class ModuleDef<LOC, NAME, VAL, KIND, DEF, TERM> extends Definition<LOC, NAME, VAL, KIND, DEF, TERM> {
        private final Ident<LOC, NAME> name;
        private final ModuleTerm<LOC, NAME, VAL, KIND, DEF, TERM> body;

        private ModuleDef(Ident<LOC, NAME> name, ModuleTerm<LOC, NAME, VAL, KIND, DEF, TERM> body) {
            this.name = name;
            this.body = body;
        }

        public Ident<LOC, NAME> getName() {
            return this.name;
        }

        public ModuleTerm<LOC, NAME, VAL, KIND, DEF, TERM> getBody() {
            return this.body;
        }
    }
}

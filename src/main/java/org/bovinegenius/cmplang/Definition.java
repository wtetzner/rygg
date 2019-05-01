package org.bovinegenius.cmplang;

public abstract class Definition<NAME, IDENT extends Ident<NAME>, VAL, KIND, DEF, TERM> {
    private Definition() {}

    public static class ValueDef<NAME, IDENT extends Ident<NAME>, VAL, KIND, DEF, TERM> extends Definition<NAME, IDENT, VAL, KIND, DEF, TERM> {
        private final IDENT name;
        private final TERM value;

        private ValueDef(IDENT name, TERM value) {
            this.name = name;
            this.value = value;
        }

        public IDENT getName() {
            return this.name;
        }

        public TERM getValue() {
            return this.value;
        }
    }

    public static class TypeDef<NAME, IDENT extends Ident<NAME>, VAL, KIND, DEF, TERM> extends Definition<NAME, IDENT, VAL, KIND, DEF, TERM> {
        private final IDENT name;
        private final KIND kind;
        private final DEF definition;

        private TypeDef(IDENT name, KIND kind, DEF definition) {
            this.name = name;
            this.kind = kind;
            this.definition = definition;
        }

        public IDENT getName() {
            return this.name;
        }

        public KIND getKind() {
            return this.kind;
        }

        public DEF getDefinition() {
            return this.definition;
        }
    }

    public static class ModuleDef<NAME, IDENT extends Ident<NAME>, VAL, KIND, DEF, TERM> extends Definition<NAME, IDENT, VAL, KIND, DEF, TERM> {
        private final IDENT name;
        private final ModuleTerm<NAME, IDENT, VAL, KIND, DEF, TERM> body;

        private ModuleDef(IDENT name, ModuleTerm<NAME, IDENT, VAL, KIND, DEF, TERM> body) {
            this.name = name;
            this.body = body;
        }

        public IDENT getName() {
            return this.name;
        }

        public ModuleTerm<NAME, IDENT, VAL, KIND, DEF, TERM> getBody() {
            return this.body;
        }
    }
}

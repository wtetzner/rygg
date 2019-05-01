package org.bovinegenius.cmplang;

public abstract class Specification<NAME, IDENT extends Ident<NAME>, VAL, KIND, DEF> {
    private Specification() {}

    public abstract IDENT getName();
    
    public static <NAME, IDENT extends Ident<NAME>, VAL, KIND, DEF> ValueSig<NAME, IDENT, VAL, KIND, DEF> valueSig(IDENT name, VAL type) {
        return new ValueSig<NAME, IDENT, VAL, KIND, DEF>(name, type);
    }

    public static <NAME, IDENT extends Ident<NAME>, VAL, KIND, DEF> TypeSig<NAME, IDENT, VAL, KIND, DEF> typeSig(IDENT name, TypeDecl<KIND, DEF> decl) {
        return new TypeSig<NAME, IDENT, VAL, KIND, DEF>(name, decl);
    }

    public static <NAME, IDENT extends Ident<NAME>, VAL, KIND, DEF> ModuleSig<NAME, IDENT, VAL, KIND, DEF> moduleSig(IDENT name, ModuleType<NAME, IDENT, VAL, KIND, DEF> type) {
        return new ModuleSig<NAME, IDENT, VAL, KIND, DEF>(name, type);
    }

    public static class ValueSig<NAME, IDENT extends Ident<NAME>, VAL, KIND, DEF> extends Specification<NAME, IDENT, VAL, KIND, DEF> {
        private final IDENT name;
        private final VAL type;

        private ValueSig(IDENT name, VAL type) {
            this.name = name;
            this.type = type;
        }

        public IDENT getName() {
            return this.name;
        }

        public VAL getType() {
            return this.type;
        }
    }

    public static class TypeSig<NAME, IDENT extends Ident<NAME>, VAL, KIND, DEF> extends Specification<NAME, IDENT, VAL, KIND, DEF> {
        private final IDENT name;
        private final TypeDecl<KIND, DEF> decl;

        private TypeSig(IDENT name, TypeDecl<KIND, DEF> decl) {
            this.name = name;
            this.decl = decl;
        }

        public IDENT getName() {
            return this.name;
        }

        public TypeDecl<KIND, DEF> getDecl() {
            return this.decl;
        }
    }

    public static class ModuleSig<NAME, IDENT extends Ident<NAME>, VAL, KIND, DEF> extends Specification<NAME, IDENT, VAL, KIND, DEF> {
        private final IDENT name;
        private final ModuleType<NAME, IDENT, VAL, KIND, DEF> type;

        private ModuleSig(IDENT name, ModuleType<NAME, IDENT, VAL, KIND, DEF> type) {
            this.name = name;
            this.type = type;
        }

        public IDENT getName() {
            return this.name;
        }

        public ModuleType<NAME, IDENT, VAL, KIND, DEF> getType() {
            return this.type;
        }
    }
}
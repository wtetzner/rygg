package org.bovinegenius.cmplang.ast.module;

public abstract class Specification<LOC, NAME, IDENT extends Ident<NAME, LOC>, VAL, KIND, DEF> {
    private Specification() {}

    public abstract IDENT getName();
    
    public static <LOC, NAME, IDENT extends Ident<NAME, LOC>, VAL, KIND, DEF> ValueSig<LOC, NAME, IDENT, VAL, KIND, DEF> valueSig(IDENT name, VAL type) {
        return new ValueSig<>(name, type);
    }

    public static <LOC, NAME, IDENT extends Ident<NAME, LOC>, VAL, KIND, DEF> TypeSig<LOC, NAME, IDENT, VAL, KIND, DEF> typeSig(IDENT name, TypeDecl<KIND, DEF> decl) {
        return new TypeSig<>(name, decl);
    }

    public static <LOC, NAME, IDENT extends Ident<NAME, LOC>, VAL, KIND, DEF> ModuleSig<LOC, NAME, IDENT, VAL, KIND, DEF> moduleSig(IDENT name, ModuleType<LOC, NAME, IDENT, VAL, KIND, DEF> type) {
        return new ModuleSig<>(name, type);
    }

    public static class ValueSig<LOC, NAME, IDENT extends Ident<NAME, LOC>, VAL, KIND, DEF> extends Specification<LOC, NAME, IDENT, VAL, KIND, DEF> {
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

    public static class TypeSig<LOC, NAME, IDENT extends Ident<NAME, LOC>, VAL, KIND, DEF> extends Specification<LOC, NAME, IDENT, VAL, KIND, DEF> {
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

    public static class ModuleSig<LOC, NAME, IDENT extends Ident<NAME, LOC>, VAL, KIND, DEF> extends Specification<LOC, NAME, IDENT, VAL, KIND, DEF> {
        private final IDENT name;
        private final ModuleType<LOC, NAME, IDENT, VAL, KIND, DEF> type;

        private ModuleSig(IDENT name, ModuleType<LOC, NAME, IDENT, VAL, KIND, DEF> type) {
            this.name = name;
            this.type = type;
        }

        public IDENT getName() {
            return this.name;
        }

        public ModuleType<LOC, NAME, IDENT, VAL, KIND, DEF> getType() {
            return this.type;
        }
    }
}
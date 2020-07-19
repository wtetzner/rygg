package org.bovinegenius.cmplang.ast.module;

public abstract class Specification<LOC, NAME, VAL, KIND, DEF> {
    private Specification() {}

    public abstract Ident<LOC, NAME> getName();
    
    public static <LOC, NAME, VAL, KIND, DEF> ValueSig<LOC, NAME, VAL, KIND, DEF> valueSig(Ident<LOC, NAME> name, VAL type) {
        return new ValueSig<>(name, type);
    }

    public static <LOC, NAME, VAL, KIND, DEF> TypeSig<LOC, NAME, VAL, KIND, DEF> typeSig(Ident<LOC, NAME> name, TypeDecl<KIND, DEF> decl) {
        return new TypeSig<>(name, decl);
    }

    public static <LOC, NAME, VAL, KIND, DEF> ModuleSig<LOC, NAME, VAL, KIND, DEF> moduleSig(Ident<LOC, NAME> name, ModuleType<LOC, NAME, VAL, KIND, DEF> type) {
        return new ModuleSig<>(name, type);
    }

    public static class ValueSig<LOC, NAME, VAL, KIND, DEF> extends Specification<LOC, NAME, VAL, KIND, DEF> {
        private final Ident<LOC, NAME> name;
        private final VAL type;

        private ValueSig(Ident<LOC, NAME> name, VAL type) {
            this.name = name;
            this.type = type;
        }

        public Ident<LOC, NAME> getName() {
            return this.name;
        }

        public VAL getType() {
            return this.type;
        }
    }

    public static class TypeSig<LOC, NAME, VAL, KIND, DEF> extends Specification<LOC, NAME, VAL, KIND, DEF> {
        private final Ident<LOC, NAME> name;
        private final TypeDecl<KIND, DEF> decl;

        private TypeSig(Ident<LOC, NAME> name, TypeDecl<KIND, DEF> decl) {
            this.name = name;
            this.decl = decl;
        }

        public Ident<LOC, NAME> getName() {
            return this.name;
        }

        public TypeDecl<KIND, DEF> getDecl() {
            return this.decl;
        }
    }

    public static class ModuleSig<LOC, NAME, VAL, KIND, DEF> extends Specification<LOC, NAME, VAL, KIND, DEF> {
        private final Ident<LOC, NAME> name;
        private final ModuleType<LOC, NAME, VAL, KIND, DEF> type;

        private ModuleSig(Ident<LOC, NAME> name, ModuleType<LOC, NAME, VAL, KIND, DEF> type) {
            this.name = name;
            this.type = type;
        }

        public Ident<LOC, NAME> getName() {
            return this.name;
        }

        public ModuleType<LOC, NAME, VAL, KIND, DEF> getType() {
            return this.type;
        }
    }
}
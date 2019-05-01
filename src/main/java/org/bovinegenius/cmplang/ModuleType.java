package org.bovinegenius.cmplang;

import org.pcollections.PVector;

public abstract class ModuleType<NAME, IDENT extends Ident<NAME>, VAL, KIND, DEF> {
    private ModuleType() {}

    public static <NAME, IDENT extends Ident<NAME>, VAL, KIND, DEF> Signature<NAME, IDENT, VAL, KIND, DEF> signature(PVector<Specification<NAME, IDENT, VAL, KIND, DEF>> specifications) {
        return new Signature<>(specifications);
    }

    public static <NAME, IDENT extends Ident<NAME>, VAL, KIND, DEF> FunctorType<NAME, IDENT, VAL, KIND, DEF> functorType(IDENT argName, ModuleType<NAME, IDENT, VAL, KIND, DEF> argType, ModuleType<NAME, IDENT, VAL, KIND, DEF> body) {
        return new FunctorType<>(argName, argType, body);
    }

    public static class Signature<NAME, IDENT extends Ident<NAME>, VAL, KIND, DEF> extends ModuleType<NAME, IDENT, VAL, KIND, DEF> {
        private final PVector<Specification<NAME, IDENT, VAL, KIND, DEF>> specifications;

        private Signature(PVector<Specification<NAME, IDENT, VAL, KIND, DEF>> specifications) {
            this.specifications = specifications;
        }

        public PVector<Specification<NAME, IDENT, VAL, KIND, DEF>> getSpecifications() {
            return this.specifications;
        }
    }

    public static class FunctorType<NAME, IDENT extends Ident<NAME>, VAL, KIND, DEF> extends ModuleType<NAME, IDENT, VAL, KIND, DEF> {
        private final IDENT argName;
        private final ModuleType<NAME, IDENT, VAL, KIND, DEF> argType;
        private final ModuleType<NAME, IDENT, VAL, KIND, DEF> body;

        private FunctorType(IDENT argName, ModuleType<NAME, IDENT, VAL, KIND, DEF> argType, ModuleType<NAME, IDENT, VAL, KIND, DEF> body) {
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

        public ModuleType<NAME, IDENT, VAL, KIND, DEF> getBody() {
            return this.body;
        }
    }
}

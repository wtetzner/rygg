package org.bovinegenius.cmplang.ast.module;

import lombok.Getter;
import org.pcollections.PVector;

import java.util.Optional;

public abstract class ModuleType<LOC, NAME, IDENT extends Ident<NAME, LOC>, VAL, KIND, DEF> {
    private ModuleType() {}

    public abstract LOC getLocation();

    public static <LOC, NAME, IDENT extends Ident<NAME, LOC>, VAL, KIND, DEF> Signature<LOC, NAME, IDENT, VAL, KIND, DEF> signature(LOC location, PVector<Specification<LOC, NAME, IDENT, VAL, KIND, DEF>> specifications) {
        return new Signature<>(location, specifications);
    }

    public static <LOC, NAME, IDENT extends Ident<NAME, LOC>, VAL, KIND, DEF> FunctorType<LOC, NAME, IDENT, VAL, KIND, DEF> functorType(LOC location, IDENT argName, ModuleType<LOC, NAME, IDENT, VAL, KIND, DEF> argType, ModuleType<LOC, NAME, IDENT, VAL, KIND, DEF> body) {
        return new FunctorType<>(location, argName, argType, body);
    }

    public static class Signature<LOC, NAME, IDENT extends Ident<NAME, LOC>, VAL, KIND, DEF> extends ModuleType<LOC, NAME, IDENT, VAL, KIND, DEF> {
        @Getter private final LOC location;
        private final PVector<Specification<LOC, NAME, IDENT, VAL, KIND, DEF>> specifications;

        private Signature(LOC location, PVector<Specification<LOC, NAME, IDENT, VAL, KIND, DEF>> specifications) {
            this.location = location;
            this.specifications = specifications;
        }

        public PVector<Specification<LOC, NAME, IDENT, VAL, KIND, DEF>> getSpecifications() {
            return this.specifications;
        }

        public Optional<Specification<LOC, NAME, IDENT, VAL, KIND, DEF>> findMatching(Specification<LOC, NAME, IDENT, VAL, KIND, DEF> spec) {
            return this.specifications.stream()
                    .filter(s -> spec.getClass().equals(s.getClass()) && spec.getName().equals(s.getName()))
                    .findFirst();
        }
    }

    public static class FunctorType<LOC, NAME, IDENT extends Ident<NAME, LOC>, VAL, KIND, DEF> extends ModuleType<LOC, NAME, IDENT, VAL, KIND, DEF> {
        @Getter private final LOC location;
        private final IDENT argName;
        private final ModuleType<LOC, NAME, IDENT, VAL, KIND, DEF> argType;
        private final ModuleType<LOC, NAME, IDENT, VAL, KIND, DEF> body;

        private FunctorType(LOC location, IDENT argName, ModuleType<LOC, NAME, IDENT, VAL, KIND, DEF> argType, ModuleType<LOC, NAME, IDENT, VAL, KIND, DEF> body) {
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

        public ModuleType<LOC, NAME, IDENT, VAL, KIND, DEF> getBody() {
            return this.body;
        }
    }
}

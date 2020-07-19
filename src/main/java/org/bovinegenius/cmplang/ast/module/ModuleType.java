package org.bovinegenius.cmplang.ast.module;

import lombok.Getter;
import org.pcollections.PVector;

import java.util.Optional;

public abstract class ModuleType<LOC, NAME, VAL, KIND, DEF> {
    private ModuleType() {}

    public abstract LOC getLocation();

    public static <LOC, NAME, VAL, KIND, DEF> Signature<LOC, NAME, VAL, KIND, DEF> signature(LOC location, PVector<Specification<LOC, NAME, VAL, KIND, DEF>> specifications) {
        return new Signature<>(location, specifications);
    }

    public static <LOC, NAME, VAL, KIND, DEF> FunctorType<LOC, NAME, VAL, KIND, DEF> functorType(LOC location, Ident<LOC, NAME> argName, ModuleType<LOC, NAME, VAL, KIND, DEF> argType, ModuleType<LOC, NAME, VAL, KIND, DEF> body) {
        return new FunctorType<>(location, argName, argType, body);
    }

    public static class Signature<LOC, NAME, VAL, KIND, DEF> extends ModuleType<LOC, NAME, VAL, KIND, DEF> {
        @Getter private final LOC location;
        private final PVector<Specification<LOC, NAME, VAL, KIND, DEF>> specifications;

        private Signature(LOC location, PVector<Specification<LOC, NAME, VAL, KIND, DEF>> specifications) {
            this.location = location;
            this.specifications = specifications;
        }

        public PVector<Specification<LOC, NAME, VAL, KIND, DEF>> getSpecifications() {
            return this.specifications;
        }

        public Optional<Specification<LOC, NAME, VAL, KIND, DEF>> findMatching(Specification<LOC, NAME, VAL, KIND, DEF> spec) {
            return this.specifications.stream()
                    .filter(s -> spec.getClass().equals(s.getClass()) && spec.getName().equals(s.getName()))
                    .findFirst();
        }
    }

    public static class FunctorType<LOC, NAME, VAL, KIND, DEF> extends ModuleType<LOC, NAME, VAL, KIND, DEF> {
        @Getter private final LOC location;
        private final Ident<LOC, NAME> argName;
        private final ModuleType<LOC, NAME, VAL, KIND, DEF> argType;
        private final ModuleType<LOC, NAME, VAL, KIND, DEF> body;

        private FunctorType(LOC location, Ident<LOC, NAME> argName, ModuleType<LOC, NAME, VAL, KIND, DEF> argType, ModuleType<LOC, NAME, VAL, KIND, DEF> body) {
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

        public ModuleType<LOC, NAME, VAL, KIND, DEF> getBody() {
            return this.body;
        }
    }
}

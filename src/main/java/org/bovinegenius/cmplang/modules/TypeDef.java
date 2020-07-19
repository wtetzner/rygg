package org.bovinegenius.cmplang.modules;

import lombok.NonNull;
import lombok.Value;

import java.util.Optional;

@Value(staticConstructor = "of")
public class TypeDef<KIND, TYPE> {
    @NonNull KIND kind;
    @NonNull Optional<TYPE> concreteType;
}

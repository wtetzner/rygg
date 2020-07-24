package org.bovinegenius.cmplang.modules;

import lombok.Getter;
import lombok.NonNull;
import org.bovinegenius.cmplang.ast.module.Path;

import java.util.Optional;

public class FunctorArg<LOC extends Comparable<LOC>, NAME, VAL extends Node<LOC, NAME, VAL>, KIND extends Node<LOC, NAME, KIND>, TYPE extends Node<LOC, NAME, TYPE>> {
    @Getter private final Optional<Path<LOC, NAME>> path;
    @Getter private final Optional<ModuleBody<LOC, NAME, VAL, KIND, TYPE>> moduleBody;
    @Getter private final Optional<Functor<LOC, NAME, VAL, KIND, TYPE>> functorBody;
    @Getter private final Optional<FunctorApplication<LOC, NAME, VAL, KIND, TYPE>> functorApplication;

    private FunctorArg(
            @NonNull final Optional<Path<LOC, NAME>> path,
            @NonNull final Optional<ModuleBody<LOC, NAME, VAL, KIND, TYPE>> moduleBody,
            @NonNull final Optional<Functor<LOC, NAME, VAL, KIND, TYPE>> functorBody,
            @NonNull final Optional<FunctorApplication<LOC, NAME, VAL, KIND, TYPE>> functorApplication
    ) {
        this.path = path;
        this.moduleBody = moduleBody;
        this.functorBody = functorBody;
        this.functorApplication = functorApplication;
    }


}

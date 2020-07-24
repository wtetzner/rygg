package org.bovinegenius.cmplang.modules;

import lombok.NonNull;
import lombok.ToString;
import org.bovinegenius.cmplang.ast.module.Subst;
import org.bovinegenius.cmplang.util.Formatted;

@ToString
public class FunctorType<LOC extends Comparable<LOC>, NAME, KIND extends Node<LOC, NAME, KIND>, TYPE extends Node<LOC, NAME, TYPE>>
        implements Substable<LOC, NAME, FunctorType<LOC, NAME, KIND, TYPE>>, Formatted {

    private final FunctorArgType<LOC, NAME, KIND, TYPE> input;
    private final FunctorOutputType<LOC, NAME, KIND, TYPE> output;

    private FunctorType(
            @NonNull final FunctorArgType<LOC, NAME, KIND, TYPE> input,
            @NonNull final FunctorOutputType<LOC, NAME, KIND, TYPE> output
    ) {
        this.input = input;
        this.output = output;
    }

    public static <LOC extends Comparable<LOC>, NAME, KIND extends Node<LOC, NAME, KIND>, TYPE extends Node<LOC, NAME, TYPE>> FunctorType<LOC, NAME, KIND, TYPE> of(
            @NonNull FunctorArgType<LOC, NAME, KIND, TYPE> input,
            @NonNull FunctorOutputType<LOC, NAME, KIND, TYPE> output
            ) {
        return new FunctorType<>(input, output);
    }

    @Override
    public FunctorType<LOC, NAME, KIND, TYPE> subst(Subst<LOC, NAME> subst) {
        return new FunctorType<>(
                this.input.subst(subst),
                this.output.subst(subst)
        );
    }

    @Override
    public String formatted(boolean inline, int indentAmount, int indentLevel) {
        return this.input.formatted(inline, indentAmount, indentLevel) + " => " + this.input.formatted(inline, indentAmount, indentLevel);
    }

}

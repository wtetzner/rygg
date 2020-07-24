package org.bovinegenius.cmplang.modules;

import lombok.NonNull;
import lombok.ToString;
import org.apache.commons.lang3.tuple.Pair;
import org.bovinegenius.cmplang.ast.module.Ident;
import org.bovinegenius.cmplang.ast.module.Subst;
import org.bovinegenius.cmplang.ast.module.error.*;
import org.bovinegenius.cmplang.util.Formatted;
import org.bovinegenius.cmplang.util.Result;

import java.util.*;

@ToString
public class ModuleBody<LOC extends Comparable<LOC>, NAME, VAL extends Node<LOC, NAME, VAL>, KIND extends Node<LOC, NAME, KIND>, TYPE extends Node<LOC, NAME, TYPE>>
        implements Substable<LOC, NAME, ModuleBody<LOC, NAME, VAL, KIND, TYPE>>, Formatted {
    private final IdentMap<LOC, NAME, TypeDef<LOC, NAME, KIND, TYPE>> typeDefs;
    private final IdentMap<LOC, NAME, VAL> valueDefs;
    private final IdentMap<LOC, NAME, ModuleDef<LOC, NAME, VAL, KIND, TYPE>> moduleDef;
    private final IdentMap<LOC, NAME, FunctorDef<LOC, NAME, VAL, KIND, TYPE>> functorDef;
    private final IdentMap<LOC, NAME, ModuleTypeDef<LOC, NAME, KIND, TYPE>> signatureDef;
    private final IdentMap<LOC, NAME, ModuleTypeDef<LOC, NAME, KIND, TYPE>> moduleTypeDef;

    private ModuleBody(
            @NonNull final IdentMap<LOC, NAME, TypeDef<LOC, NAME, KIND, TYPE>> typeDefs,
            @NonNull final IdentMap<LOC, NAME, VAL> valueDefs,
            @NonNull final IdentMap<LOC, NAME, ModuleDef<LOC, NAME, VAL, KIND, TYPE>> moduleDef,
            @NonNull final IdentMap<LOC, NAME, FunctorDef<LOC, NAME, VAL, KIND, TYPE>> functorDef,
            @NonNull final IdentMap<LOC, NAME, ModuleTypeDef<LOC, NAME, KIND, TYPE>> signatureDef,
            @NonNull final IdentMap<LOC, NAME, ModuleTypeDef<LOC, NAME, KIND, TYPE>> moduleTypeDef
    ) {
        this.typeDefs = typeDefs;
        this.valueDefs = valueDefs;
        this.moduleDef = moduleDef;
        this.functorDef = functorDef;
        this.signatureDef = signatureDef;
        this.moduleTypeDef = moduleTypeDef;
    }

    public static <LOC extends Comparable<LOC>, NAME, VAL extends Node<LOC, NAME, VAL>, KIND extends Node<LOC, NAME, KIND>, TYPE extends Node<LOC, NAME, TYPE>> ModuleBody<LOC, NAME, VAL, KIND, TYPE> empty() {
        return new ModuleBody<>(
                IdentMap.empty(),
                IdentMap.<LOC, NAME, VAL>empty(),
                IdentMap.empty(),
                IdentMap.empty(),
                IdentMap.empty(),
                IdentMap.empty()
        );
    }

    public Result<ModuleBody<LOC, NAME, VAL, KIND, TYPE>, List<TypeError<LOC>>> withTypeDef(@NonNull Ident<LOC, NAME> ident, TypeDef<LOC, NAME, KIND, TYPE> value) {
        Optional<Ident<LOC, NAME>> existing = this.typeDefs.ident(ident.getName());
        if (existing.isPresent()) {
            return Result.err(
                    Collections.singletonList(
                            RepeatedTypeName.<LOC, NAME>builder()
                                    .location(ident.getLocation())
                                    .original(existing.get())
                                    .duplicate(ident)
                                    .build()
                    ));
        }
        return Result.ok(new ModuleBody<>(this.typeDefs.plus(ident, value), this.valueDefs, this.moduleDef, this.functorDef, this.signatureDef, this.moduleTypeDef));
    }

    public Result<ModuleBody<LOC, NAME, VAL, KIND, TYPE>, List<TypeError<LOC>>> withValueDef(@NonNull Ident<LOC, NAME> ident, VAL value) {
        Optional<Ident<LOC, NAME>> existing = this.valueDefs.ident(ident.getName());
        if (existing.isPresent()) {
            return Result.err(
                    Collections.singletonList(
                            RepeatedValueName.<LOC, NAME>builder()
                                    .location(ident.getLocation())
                                    .original(existing.get())
                                    .duplicate(ident)
                                    .build()
                    ));
        }
        return Result.ok(new ModuleBody<>(this.typeDefs, this.valueDefs.plus(ident, value), this.moduleDef, this.functorDef, this.signatureDef, this.moduleTypeDef));
    }

    public Result<ModuleBody<LOC, NAME, VAL, KIND, TYPE>, List<TypeError<LOC>>> withModuleDef(@NonNull Ident<LOC, NAME> ident, ModuleDef<LOC, NAME, VAL, KIND, TYPE> value) {
        Optional<Ident<LOC, NAME>> existing = this.moduleDef.ident(ident.getName());
        if (existing.isPresent()) {
            return Result.err(
                    Collections.singletonList(
                            RepeatedModuleName.<LOC, NAME>builder()
                                    .location(ident.getLocation())
                                    .original(existing.get())
                                    .duplicate(ident)
                                    .build()
                    ));
        }
        return Result.ok(new ModuleBody<>(this.typeDefs, this.valueDefs, this.moduleDef.plus(ident, value), this.functorDef, this.signatureDef, this.moduleTypeDef));
    }

    public Result<ModuleBody<LOC, NAME, VAL, KIND, TYPE>, List<TypeError<LOC>>> withFunctorDef(@NonNull Ident<LOC, NAME> ident, FunctorDef<LOC, NAME, VAL, KIND, TYPE> value) {
        Optional<Ident<LOC, NAME>> existing = this.functorDef.ident(ident.getName());
        if (existing.isPresent()) {
            return Result.err(
                    Collections.singletonList(
                            RepeatedFunctorName.<LOC, NAME>builder()
                                    .location(ident.getLocation())
                                    .original(existing.get())
                                    .duplicate(ident)
                                    .build()
                    ));
        }
        return Result.ok(new ModuleBody<>(this.typeDefs, this.valueDefs, this.moduleDef, this.functorDef.plus(ident, value), this.signatureDef, this.moduleTypeDef));
    }

    public Result<ModuleBody<LOC, NAME, VAL, KIND, TYPE>, List<TypeError<LOC>>> withSignatureDef(@NonNull Ident<LOC, NAME> ident, ModuleTypeDef<LOC, NAME, KIND, TYPE> value) {
        Optional<Ident<LOC, NAME>> existing = this.signatureDef.ident(ident.getName());
        if (existing.isPresent()) {
            return Result.err(
                    Collections.singletonList(
                            RepeatedSignatureName.<LOC, NAME>builder()
                                    .location(ident.getLocation())
                                    .original(existing.get())
                                    .duplicate(ident)
                                    .build()
                    ));
        }
        return Result.ok(new ModuleBody<>(this.typeDefs, this.valueDefs, this.moduleDef, this.functorDef, this.signatureDef.plus(ident, value), this.moduleTypeDef));
    }

    public Optional<VAL> getValDef(@NonNull Ident<LOC, NAME> ident) {
        return this.valueDefs.get(ident);
    }

    public Optional<VAL> getValDef(@NonNull NAME name) {
        return this.valueDefs.get(name);
    }

    public Optional<TypeDef<LOC, NAME, KIND, TYPE>> getTypeDef(@NonNull Ident<LOC, NAME> ident) {
        return this.typeDefs.get(ident);
    }

    public Optional<TypeDef<LOC, NAME, KIND, TYPE>> getTypeDef(@NonNull NAME name) {
        return this.typeDefs.get(name);
    }

    public Optional<ModuleDef<LOC, NAME, VAL, KIND, TYPE>> getModuleDef(@NonNull Ident<LOC, NAME> ident) {
        return this.moduleDef.get(ident);
    }

    public Optional<ModuleDef<LOC, NAME, VAL, KIND, TYPE>> getModuleDef(@NonNull NAME name) {
        return this.moduleDef.get(name);
    }

    public Optional<FunctorDef<LOC, NAME, VAL, KIND, TYPE>> getFunctorDef(@NonNull Ident<LOC, NAME> ident) {
        return this.functorDef.get(ident);
    }

    public Optional<FunctorDef<LOC, NAME, VAL, KIND, TYPE>> getFunctorDef(@NonNull NAME name) {
        return this.functorDef.get(name);
    }

    public Optional<ModuleTypeDef<LOC, NAME, KIND, TYPE>> getSignatureDef(@NonNull Ident<LOC, NAME> ident) {
        return this.signatureDef.get(ident);
    }

    public Optional<ModuleTypeDef<LOC, NAME, KIND, TYPE>> getSignatureDef(@NonNull NAME name) {
        return this.signatureDef.get(name);
    }

    @Override
    public ModuleBody<LOC, NAME, VAL, KIND, TYPE> subst(Subst<LOC, NAME> subst) {
        return new ModuleBody<>(
                this.typeDefs.subst(subst),
                this.valueDefs.subst(subst),
                this.moduleDef.subst(subst),
                this.functorDef.subst(subst),
                this.signatureDef.subst(subst),
                this.moduleTypeDef.subst(subst)
        );
    }

    @Override
    public String formatted(boolean inline, int indentAmount, int indentLevel) {
        StringBuilder sb = new StringBuilder();
        sb.append("{").append(inline ? " " : "\n");
        boolean first = true;
        for (Formatted formatted : allEntries()) {
            if (first) {
                first = false;
            } else {
                sb.append(inline ? " " : "\n");
            }
            if (!inline) {
                sb.append(Formatted.indent(indentAmount, indentLevel + 1));
            }
            sb.append(formatted.formatted(inline, indentAmount, indentLevel + 1));
        }
        if (inline) {
            sb.append(" ");
        } else {
            sb.append(Formatted.indent(indentAmount, indentLevel));
        }
        sb.append("}");
        return sb.toString();
    }

    private Collection<Formatted> allEntries() {
        Collection<Map.Entry<Ident<LOC, NAME>, ? extends Formatted>> results = new TreeSet<>((l, r) -> l.getKey().compareTo(r.getKey()));
        for (Map.Entry<Ident<LOC, NAME>, TypeDef<LOC, NAME, KIND, TYPE>> entry : this.typeDefs.entries()) {
            results.add(Pair.of(
                    entry.getKey(),
                    (boolean inline, int indentAmount, int indentLevel) -> {
                        StringBuilder sb = new StringBuilder();
                        sb.append("type ")
                                .append(entry.getKey().formatted(true, indentAmount, indentLevel))
                                .append(" ")
                                .append(entry.getValue().getKind().formatted(true, indentAmount, indentLevel));
                        if (entry.getValue().getConcreteType().isPresent()) {
                            sb.append(" = ").append(entry.getValue().getConcreteType().get().formatted(inline, indentAmount, indentLevel));
                        }
                        return sb.toString();
                    }
            ));
        }

        for (Map.Entry<Ident<LOC, NAME>, VAL> entry : this.valueDefs.entries()) {
            results.add(Pair.of(
                    entry.getKey(),
                    (boolean inline, int indentAmount, int indentLevel) -> {
                        StringBuilder sb = new StringBuilder();
                        sb.append("let ")
                                .append(entry.getKey().formatted(true, indentAmount, indentLevel))
                                .append(" = ")
                                .append(entry.getValue().formatted(inline, indentAmount, indentLevel));
                        return sb.toString();
                    }
            ));
        }

        this.moduleDef.entries().forEach(results::add);
        this.functorDef.entries().forEach(results::add);
        this.moduleTypeDef.entries().forEach(results::add);
        this.signatureDef.entries().forEach(results::add);

        List<Formatted> finalResults = new ArrayList<>();
        for (Map.Entry<Ident<LOC, NAME>, ? extends Formatted> entry : results) {
            finalResults.add(entry.getValue());
        }
        return finalResults;
    }

}

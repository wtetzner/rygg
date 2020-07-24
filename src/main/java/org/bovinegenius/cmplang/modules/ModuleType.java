package org.bovinegenius.cmplang.modules;

import lombok.NonNull;
import lombok.ToString;
import org.apache.commons.lang3.tuple.Pair;
import org.bovinegenius.cmplang.ast.module.Ident;
import org.bovinegenius.cmplang.ast.module.Subst;
import org.bovinegenius.cmplang.ast.module.error.RepeatedModuleTypeName;
import org.bovinegenius.cmplang.ast.module.error.RepeatedTypeName;
import org.bovinegenius.cmplang.ast.module.error.RepeatedValueName;
import org.bovinegenius.cmplang.ast.module.error.TypeError;
import org.bovinegenius.cmplang.util.Formatted;
import org.bovinegenius.cmplang.util.Result;

import java.util.*;

@ToString
public class ModuleType<LOC extends Comparable<LOC>, NAME, KIND extends Node<LOC, NAME, KIND>, TYPE extends Node<LOC, NAME, TYPE>>
        implements Substable<LOC, NAME, ModuleType<LOC, NAME, KIND, TYPE>>, Formatted {
    private final IdentMap<LOC, NAME, TypeDef<LOC, NAME, KIND, TYPE>> typeDecl;
    private final IdentMap<LOC, NAME, TYPE> valType;
    private final IdentMap<LOC, NAME, ModuleType<LOC, NAME, KIND, TYPE>> moduleType;

    private ModuleType(
            final IdentMap<LOC, NAME, TypeDef<LOC, NAME, KIND, TYPE>> typeDecl,
            final IdentMap<LOC, NAME, TYPE> valType,
            final IdentMap<LOC, NAME, ModuleType<LOC, NAME, KIND, TYPE>> moduleType
    ) {
        this.typeDecl = typeDecl;
        this.valType = valType;
        this.moduleType = moduleType;
    }

    public static <LOC extends Comparable<LOC>, NAME, KIND extends Node<LOC, NAME, KIND>, TYPE extends Node<LOC, NAME, TYPE>> ModuleType<LOC, NAME, KIND, TYPE> empty() {
        return new ModuleType<>(
                IdentMap.<LOC, NAME, TypeDef<LOC, NAME, KIND, TYPE>>empty(),
                IdentMap.empty(),
                IdentMap.empty()
        );
    }

    @Override
    public ModuleType<LOC, NAME, KIND, TYPE> subst(Subst<LOC, NAME> subst) {
        return new ModuleType<>(
                this.typeDecl.subst(subst),
                this.valType.subst(subst),
                this.moduleType.subst(subst)
        );
    }

    public Result<ModuleType<LOC, NAME, KIND, TYPE>, List<TypeError<LOC>>> withTypeDecl(@NonNull Ident<LOC, NAME> ident, TypeDef<LOC, NAME, KIND, TYPE> decl) {
        Optional<Ident<LOC, NAME>> existing = this.typeDecl.ident(ident.getName());
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
        return Result.ok(new ModuleType<>(this.typeDecl.plus(ident, decl), this.valType, this.moduleType));
    }

    public Result<ModuleType<LOC, NAME, KIND, TYPE>, List<TypeError<LOC>>> withValType(@NonNull Ident<LOC, NAME> ident, TYPE type) {
        Optional<Ident<LOC, NAME>> existing = this.valType.ident(ident.getName());
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
        return Result.ok(new ModuleType<>(this.typeDecl, this.valType.plus(ident, type), this.moduleType));
    }

    public Result<ModuleType<LOC, NAME, KIND, TYPE>, List<TypeError<LOC>>> withModuleType(@NonNull Ident<LOC, NAME> ident, ModuleType<LOC, NAME, KIND, TYPE> modType) {
        Optional<Ident<LOC, NAME>> existing = this.moduleType.ident(ident.getName());
        if (existing.isPresent()) {
            return Result.err(
                    Collections.singletonList(
                            RepeatedModuleTypeName.<LOC, NAME>builder()
                                    .location(ident.getLocation())
                                    .original(existing.get())
                                    .duplicate(ident)
                                    .build()
                    ));
        }
        return Result.ok(new ModuleType<>(this.typeDecl, this.valType, this.moduleType.plus(ident, modType)));
    }

    public Optional<TypeDef<LOC, NAME, KIND, TYPE>> getTypeDecl(@NonNull NAME name) {
        return this.typeDecl.get(name);
    }

    public Optional<TYPE> getValType(@NonNull NAME name) {
        return this.valType.get(name);
    }

    public Optional<ModuleType<LOC, NAME, KIND, TYPE>> getModuleType(@NonNull NAME name) {
        return this.moduleType.get(name);
    }

    public Iterable<Map.Entry<Ident<LOC, NAME>, TypeDef<LOC, NAME, KIND, TYPE>>> getTypeDecls() {
        return this.typeDecl.entries();
    }

    public Iterable<Map.Entry<Ident<LOC, NAME>, TYPE>> getValTypes() {
        return this.valType.entries();
    }

    public Iterable<Map.Entry<Ident<LOC, NAME>, ModuleType<LOC, NAME, KIND, TYPE>>> getModuleTypes() {
        return this.moduleType.entries();
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
        for (Map.Entry<Ident<LOC, NAME>, TypeDef<LOC, NAME, KIND, TYPE>> entry : this.typeDecl.entries()) {
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

        for (Map.Entry<Ident<LOC, NAME>, TYPE> entry : this.valType.entries()) {
            results.add(Pair.of(
                    entry.getKey(),
                    (boolean inline, int indentAmount, int indentLevel) -> {
                        StringBuilder sb = new StringBuilder();
                        sb.append("val ")
                                .append(entry.getKey().formatted(true, indentAmount, indentLevel))
                                .append(" : ")
                                .append(entry.getValue().formatted(inline, indentAmount, indentLevel));
                        return sb.toString();
                    }
            ));
        }

        this.moduleType.entries().forEach(results::add);

        List<Formatted> finalResults = new ArrayList<>();
        for (Map.Entry<Ident<LOC, NAME>, ? extends Formatted> entry : results) {
            finalResults.add(entry.getValue());
        }
        return finalResults;
    }

}

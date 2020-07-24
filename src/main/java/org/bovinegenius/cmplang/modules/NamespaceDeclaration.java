package org.bovinegenius.cmplang.modules;

import lombok.Getter;
import lombok.NonNull;
import lombok.ToString;
import org.bovinegenius.cmplang.ast.module.Ident;
import org.bovinegenius.cmplang.ast.module.Subst;
import org.bovinegenius.cmplang.ast.module.error.RepeatedModuleName;
import org.bovinegenius.cmplang.ast.module.error.RepeatedSignatureName;
import org.bovinegenius.cmplang.ast.module.error.TypeError;
import org.bovinegenius.cmplang.util.Result;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;

@ToString
public class NamespaceDeclaration<LOC extends Comparable<LOC>, NAME, VAL extends Node<LOC, NAME, VAL>, KIND extends Node<LOC, NAME, KIND>, TYPE extends Node<LOC, NAME, TYPE>>
        implements NamedNode<LOC, NAME, NamespaceDeclaration<LOC, NAME, VAL, KIND, TYPE>> {
    @Getter private final Ident<LOC, NAME> ident;
    private final IdentMap<LOC, NAME, ModuleDef<LOC, NAME, VAL, KIND, TYPE>> moduleDef;
    private final IdentMap<LOC, NAME, ModuleType<LOC, NAME, KIND, TYPE>> signatureDef;

    private NamespaceDeclaration(
            final Ident<LOC, NAME> ident,
            final IdentMap<LOC, NAME, ModuleDef<LOC, NAME, VAL, KIND, TYPE>> moduleDef,
            final IdentMap<LOC, NAME, ModuleType<LOC, NAME, KIND, TYPE>> signatureDef
    ) {
        this.ident = ident;
        this.moduleDef = moduleDef;
        this.signatureDef = signatureDef;
    }

    public static <LOC extends Comparable<LOC>, NAME, VAL extends Node<LOC, NAME, VAL>, KIND extends Node<LOC, NAME, KIND>, TYPE extends Node<LOC, NAME, TYPE>> NamespaceDeclaration<LOC, NAME, VAL, KIND, TYPE> empty(@NonNull Ident<LOC, NAME> ident) {
        return new NamespaceDeclaration<>(
                ident,
                IdentMap.empty(),
                IdentMap.empty()
        );
    }

    public Result<NamespaceDeclaration<LOC, NAME, VAL, KIND, TYPE>, List<TypeError<LOC>>> withModuleDef(@NonNull Ident<LOC, NAME> ident, ModuleDef<LOC, NAME, VAL, KIND, TYPE> modDef) {
        Optional<Ident<LOC, NAME>> existingModule = this.moduleDef.ident(ident.getName());
        if (existingModule.isPresent()) {
            return Result.err(
                    Collections.singletonList(
                            RepeatedModuleName.<LOC, NAME>builder()
                                    .location(ident.getLocation())
                                    .original(existingModule.get())
                                    .duplicate(ident)
                                    .build()
                    ));
        }
        return Result.ok(new NamespaceDeclaration<>(this.ident, this.moduleDef.plus(ident, modDef), this.signatureDef));
    }

    public Result<NamespaceDeclaration<LOC, NAME, VAL, KIND, TYPE>, List<TypeError<LOC>>> withSignataureDef(@NonNull Ident<LOC, NAME> ident, ModuleType<LOC, NAME, KIND, TYPE> sigDef) {
        Optional<Ident<LOC, NAME>> existingSignature = this.signatureDef.ident(ident.getName());
        if (existingSignature.isPresent()) {
            return Result.err(
                    Collections.singletonList(
                            RepeatedSignatureName.<LOC, NAME>builder()
                                    .location(ident.getLocation())
                                    .original(existingSignature.get())
                                    .duplicate(ident)
                                    .build()
                    ));
        }
        return Result.ok(new NamespaceDeclaration<>(this.ident, this.moduleDef, this.signatureDef.plus(ident, sigDef)));
    }

    public Optional<ModuleDef<LOC, NAME, VAL, KIND, TYPE>> getModuleDef(@NonNull Ident<LOC, NAME> ident) {
        return this.moduleDef.get(ident);
    }

    public Optional<ModuleDef<LOC, NAME, VAL, KIND, TYPE>> getModuleDef(@NonNull NAME name) {
        return this.moduleDef.get(name);
    }

    public Optional<ModuleType<LOC, NAME, KIND, TYPE>> getSignatureDef(@NonNull Ident<LOC, NAME> ident) {
        return this.signatureDef.get(ident);
    }

    public Optional<ModuleType<LOC, NAME, KIND, TYPE>> getSignatureDef(@NonNull NAME name) {
        return this.signatureDef.get(name);
    }

    public Iterable<Map.Entry<Ident<LOC, NAME>, ModuleDef<LOC, NAME, VAL, KIND, TYPE>>> getModuleDefs() {
        return this.moduleDef.entries();
    }

    public Iterable<Map.Entry<Ident<LOC, NAME>, ModuleType<LOC, NAME, KIND, TYPE>>> getSignatureDefs() {
        return this.signatureDef.entries();
    }

    @Override
    public NamespaceDeclaration<LOC, NAME, VAL, KIND, TYPE> subst(Subst<LOC, NAME> subst) {
        return new NamespaceDeclaration<>(
                this.ident,
                this.moduleDef.subst(subst),
                this.signatureDef.subst(subst)
        );
    }

    @Override
    public LOC getLocation() {
        return this.getIdent().getLocation();
    }

}

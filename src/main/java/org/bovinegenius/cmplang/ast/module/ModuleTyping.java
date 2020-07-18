package org.bovinegenius.cmplang.ast.module;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Value;
import org.apache.commons.lang3.tuple.Pair;
import org.bovinegenius.cmplang.ast.module.ModuleTerm.LongIdent;
import org.bovinegenius.cmplang.ast.module.ModuleType.FunctorType;
import org.bovinegenius.cmplang.ast.module.ModuleType.Signature;
import org.bovinegenius.cmplang.ast.module.Specification.ModuleSig;
import org.bovinegenius.cmplang.ast.module.Specification.TypeSig;
import org.bovinegenius.cmplang.ast.module.Specification.ValueSig;
import org.bovinegenius.cmplang.ast.module.error.*;
import org.bovinegenius.cmplang.util.Result;
import org.pcollections.HashTreePMap;
import org.pcollections.PMap;
import org.pcollections.PVector;
import org.pcollections.TreePVector;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;

public class ModuleTyping<LOC, NAME, IDENT extends Ident<NAME, LOC>, TERM, VAL, DEF, KIND> {
    private final ModuleSyntax<LOC, NAME, IDENT, TERM, VAL, DEF, KIND> moduleSyntax;
    private final CoreTyping<LOC, NAME, IDENT, TERM, VAL, DEF, KIND> coreTyping;
    private final CoreSyntax<LOC, NAME, IDENT, TERM, VAL, DEF, KIND> coreSyntax;

    private ModuleTyping(
            ModuleSyntax<LOC, NAME, IDENT, TERM, VAL, DEF, KIND> moduleSyntax,
            CoreTyping<LOC, NAME, IDENT, TERM, VAL, DEF, KIND> coreTyping,
            CoreSyntax<LOC, NAME, IDENT, TERM, VAL, DEF, KIND> coreSyntax) {
        this.moduleSyntax = moduleSyntax;
        this.coreTyping = coreTyping;
        this.coreSyntax = coreSyntax;
    }

    public Result<ModuleType<LOC, NAME, IDENT, VAL, KIND, DEF>, List<TypeError<LOC>>> typeModule(Env<LOC, NAME, IDENT, TERM, VAL, DEF, KIND> env, ModuleTerm<LOC, NAME, IDENT, VAL, KIND, DEF, TERM> modTerm) {
        if (modTerm instanceof LongIdent) {
            LongIdent<LOC, NAME, IDENT, VAL, KIND, DEF, TERM> ident = (LongIdent<LOC, NAME, IDENT, VAL, KIND, DEF, TERM>)modTerm;
            Optional<ModuleType<LOC, NAME, IDENT, VAL, KIND, DEF>> module = env.findModule(ident.getPath());
            return Result.ok(strengthenModtype(ident.getPath(), module.get()));
        } else if (modTerm instanceof ModuleTerm.Structure) {
            ModuleTerm.Structure<LOC, NAME, IDENT, VAL, KIND, DEF, TERM> structure = (ModuleTerm.Structure<LOC, NAME, IDENT, VAL, KIND, DEF, TERM>)modTerm;
            Result<PVector<Specification<LOC, NAME, IDENT, VAL, KIND, DEF>>, List<TypeError<LOC>>> typedStr = typeStructure(env, structure.getDefinitions());
            return typedStr.map(s -> ModuleType.signature(modTerm.getLocation(), s));
        } else if (modTerm instanceof ModuleTerm.Functor) {
            ModuleTerm.Functor<LOC, NAME, IDENT, VAL, KIND, DEF, TERM> func = (ModuleTerm.Functor<LOC, NAME, IDENT, VAL, KIND, DEF, TERM>)modTerm;
            Result<Void, List<TypeError<LOC>>> checkResult = checkModType(env, func.getArgType());
            if (checkResult.isErr()) {
                return checkResult.mapErr(Function.identity());
            }
            Result<ModuleType<LOC, NAME, IDENT, VAL, KIND, DEF>, List<TypeError<LOC>>> typedMod = typeModule(env.withModule(func.getArgName(), func.getArgType()), func.getBody());
            if (typedMod.isErr()) {
                return typedMod;
            }
            return Result.ok(ModuleType.functorType(func.getLocation(), func.getArgName(), func.getArgType(), typedMod.get()));
        } else if (modTerm instanceof ModuleTerm.Apply) {
            ModuleTerm.Apply<LOC, NAME, IDENT, VAL, KIND, DEF, TERM> apply = (ModuleTerm.Apply<LOC, NAME, IDENT, VAL, KIND, DEF, TERM>)modTerm;
            if (apply.getArg() instanceof LongIdent) {
                LongIdent<LOC, NAME, IDENT, VAL, KIND, DEF, TERM> longIdent = (LongIdent<LOC, NAME, IDENT, VAL, KIND, DEF, TERM>)apply.getArg();
                Result<ModuleType<LOC, NAME, IDENT, VAL, KIND, DEF>, List<TypeError<LOC>>> typeResult = typeModule(env, apply.getFunctor());
                if (typeResult.isErr()) {
                    return typeResult.map(Function.identity());
                }
                if (typeResult.get() instanceof FunctorType) {
                    ModuleTerm.Functor<LOC, NAME, IDENT, VAL, KIND, DEF, TERM> func = (ModuleTerm.Functor<LOC, NAME, IDENT, VAL, KIND, DEF, TERM>)modTerm;
                    Result<ModuleType<LOC, NAME, IDENT, VAL, KIND, DEF>, List<TypeError<LOC>>> argTypeResult = typeModule(env, apply.getArg());
                    if (argTypeResult.isErr()) {
                        return argTypeResult.map(Function.identity());
                    }
                    Result<Void, List<TypeError<LOC>>> modtypeMatchResult = modtypeMatch(env, argTypeResult.get(), func.getArgType());
                    if (modtypeMatchResult.isErr()) {
                        return modtypeMatchResult.mapErr(Function.identity());
                    }
                    Subst<LOC, NAME, IDENT> subst = Subst.<LOC, NAME, IDENT>identity().with(func.getArgName(), longIdent.getPath());
                    return Result.ok(moduleSyntax.substModuleType(func.getArgType(), subst));
                } else {
                    return Result.err(Collections.singletonList(
                            ApplicationOfANonFunctor.<LOC, NAME, IDENT, VAL, KIND, DEF, TERM>builder()
                                    .location(apply.getLocation())
                                    .apply(apply)
                                    .build()
                    ));
                }
            } else {
                return Result.err(Collections.singletonList(
                        ApplicationOfFunctorToNonPath.<LOC, NAME, IDENT, VAL, KIND, DEF, TERM>builder()
                                .location(apply.getLocation())
                                .apply(apply)
                                .build()
                ));
            }
        } else if (modTerm instanceof ModuleTerm.Constraint) {
            ModuleTerm.Constraint<LOC, NAME, IDENT, VAL, KIND, DEF, TERM> constraint = (ModuleTerm.Constraint<LOC, NAME, IDENT, VAL, KIND, DEF, TERM>)modTerm;
            Result<Void, List<TypeError<LOC>>> checkResult = checkModType(env, constraint.getType());
            if (checkResult.isErr())  {
                return checkResult.mapErr(Function.identity());
            }
            Result<ModuleType<LOC, NAME, IDENT, VAL, KIND, DEF>, List<TypeError<LOC>>> modtypeResult = typeModule(env, constraint.getModule());
            if (modtypeResult.isErr()) {
                return modtypeResult.mapErr(Function.identity());
            }
            Result<Void, List<TypeError<LOC>>> modtypeMatchResult = modtypeMatch(env, modtypeResult.get(), constraint.getType());
            if (modtypeMatchResult.isErr()) {
                return modtypeResult.mapErr(Function.identity());
            }
            return Result.ok(constraint.getType());
        }
        throw new RuntimeException(String.format("Unknown module term (%s): %s", modTerm.getClass().getCanonicalName(), modTerm));
    }

    private Result<PVector<Specification<LOC, NAME, IDENT, VAL, KIND, DEF>>, List<TypeError<LOC>>> typeStructure(
            Env<LOC, NAME, IDENT, TERM, VAL, DEF, KIND> env,
            PVector<Definition<LOC, NAME, IDENT, VAL, KIND, DEF, TERM>> definitions
    ) {
        TypeErrorList<LOC> typeErrors = new TypeErrorList<>();
        SeenIdentifiers<LOC, NAME, IDENT> seen = SeenIdentifiers.empty();

        PVector<Specification<LOC, NAME, IDENT, VAL, KIND, DEF>> result = TreePVector.empty();
        for (Definition<LOC, NAME, IDENT, VAL, KIND, DEF, TERM> definition : definitions) {
            Specification<LOC, NAME, IDENT, VAL, KIND, DEF> spec;
            if (definition instanceof Definition.ValueDef) {
                Definition.ValueDef<LOC, NAME, IDENT, VAL, KIND, DEF, TERM> def = (Definition.ValueDef<LOC, NAME, IDENT, VAL, KIND, DEF, TERM>)definition;
                if (seen.containsValue(def.getName())) {
                    typeErrors.add(RepeatedValueName.<LOC, NAME, IDENT>builder()
                            .location(def.getName().getLocation())
                            .original(seen.getValue(def.getName()).get())
                            .duplicate(def.getName())
                            .build());
                } else {
                    seen = seen.plusValue(def.getName());
                }
                Result<VAL, TypeError<LOC>> typeResult = coreTyping.typeTerm(env, def.getValue());
                typeErrors.addResult(typeResult);
                if (typeResult.isOk()) {
                    result = result.plus(Specification.valueSig(def.getName(), typeResult.get()));
                }
            } else if (definition instanceof Definition.ModuleDef) {
                Definition.ModuleDef<LOC, NAME, IDENT, VAL, KIND, DEF, TERM> def = (Definition.ModuleDef<LOC, NAME, IDENT, VAL, KIND, DEF, TERM>)definition;
                if (seen.containsModule(def.getName())) {
                    typeErrors.add(RepeatedModuleName.<LOC, NAME, IDENT>builder()
                            .location(def.getName().getLocation())
                            .original(seen.getModule(def.getName()).get())
                            .duplicate(def.getName())
                            .build());
                } else {
                    seen = seen.plusModule(def.getName());
                }
                Result<ModuleType<LOC, NAME, IDENT, VAL, KIND, DEF>, List<TypeError<LOC>>> modTypeResult = typeModule(env, def.getBody());
                typeErrors.addResults(modTypeResult);
                if (modTypeResult.isOk()) {
                    result = result.plus(Specification.moduleSig(def.getName(), modTypeResult.get()));
                }
            } else if (definition instanceof Definition.TypeDef) {
                Definition.TypeDef<LOC, NAME, IDENT, VAL, KIND, DEF, TERM> def = (Definition.TypeDef<LOC, NAME, IDENT, VAL, KIND, DEF, TERM>)definition;
                if (seen.containsType(def.getName())) {
                    typeErrors.add(RepeatedTypeName.<LOC, NAME, IDENT>builder()
                            .location(def.getName().getLocation())
                            .original(seen.getType(def.getName()).get())
                            .duplicate(def.getName())
                            .build());
                } else {
                    seen = seen.plusType(def.getName());
                }
                Result<Void, TypeError<LOC>> typeResult = coreTyping.checkKind(env, def.getKind());
                typeErrors.addResult(typeResult);
                KIND defKind = coreTyping.kindDeftype(env, def.getDefinition());
                KIND kind = def.getKind();
                if (!coreTyping.kindMatch(env, defKind, kind)) {
                    typeErrors.add(KindMismatchInTypeDefinition.<LOC, KIND>builder()
                            .location(def.getName().getLocation())
                            .kind(kind)
                            .definitionKind(defKind)
                            .build());
                } else {
                    result = result.plus(Specification.typeSig(def.getName(), TypeDecl.of(kind, def.getDefinition())));
                }
            }
        }
        if (typeErrors.isEmpty()) {
            return Result.ok(result);
        } else {
            return Result.err(typeErrors);
        }
    }
    
    private Result<Void, List<TypeError<LOC>>> modtypeMatch(Env<LOC, NAME, IDENT, TERM, VAL, DEF, KIND> env, ModuleType<LOC, NAME, IDENT, VAL, KIND, DEF> modType1, ModuleType<LOC, NAME, IDENT, VAL, KIND, DEF> modType2) {
        if (modType1 instanceof Signature && modType2 instanceof Signature) {
            Signature<LOC, NAME, IDENT, VAL, KIND, DEF> sig1 = (Signature<LOC, NAME, IDENT, VAL, KIND, DEF>)modType1;
            Signature<LOC, NAME, IDENT, VAL, KIND, DEF> sig2 = (Signature<LOC, NAME, IDENT, VAL, KIND, DEF>)modType2;
            Result<List<Pair<Specification<LOC, NAME, IDENT, VAL, KIND, DEF>, Specification<LOC, NAME, IDENT, VAL, KIND, DEF>>>, List<TypeError<LOC>>> pairedResult = pairSignatureComponents(sig1, sig2);
            if (pairedResult.isErr()) {
                return pairedResult.mapErr(Function.identity());
            }
            List<Pair<Specification<LOC, NAME, IDENT, VAL, KIND, DEF>, Specification<LOC, NAME, IDENT, VAL, KIND, DEF>>> pairedComponents = pairedResult.get();
            Subst<LOC, NAME, IDENT> subst = Subst.identity();
            for (Pair<Specification<LOC, NAME, IDENT, VAL, KIND, DEF>, Specification<LOC, NAME, IDENT, VAL, KIND, DEF>> entry : pairedComponents) {
                subst = subst.with(entry.getRight().getName(), Path.of(entry.getLeft().getName()));
            }
            Env<LOC, NAME, IDENT, TERM, VAL, DEF, KIND> extEnv = env.withSignature(sig1);
            TypeErrorList<LOC> typeErrors = new TypeErrorList<>();
            for (Pair<Specification<LOC, NAME, IDENT, VAL, KIND, DEF>, Specification<LOC, NAME, IDENT, VAL, KIND, DEF>> entry : pairedComponents) {
                Result<Void, List<TypeError<LOC>>> specificationMatchResult = specificationMatch(extEnv, subst, entry);
                if (specificationMatchResult.isErr()) {
                    typeErrors.addResults(specificationMatchResult);
                }
            }
            if (!typeErrors.isEmpty()) {
                return Result.err(typeErrors);
            } else {
                return Result.ok();
            }
        } else if (modType1 instanceof FunctorType && modType2 instanceof FunctorType) {
            FunctorType<LOC, NAME, IDENT, VAL, KIND, DEF> func1 = (FunctorType<LOC, NAME, IDENT, VAL, KIND, DEF>)modType1;
            FunctorType<LOC, NAME, IDENT, VAL, KIND, DEF> func2 = (FunctorType<LOC, NAME, IDENT, VAL, KIND, DEF>)modType2;

            Subst<LOC, NAME, IDENT> subst = Subst.<LOC, NAME, IDENT>identity().with(func1.getArgName(), Path.of(func2.getArgName()));
            ModuleType<LOC, NAME, IDENT, VAL, KIND, DEF> bodySubst = moduleSyntax.substModuleType(func1.getBody(), subst);
            Result<Void, List<TypeError<LOC>>> modMatchResult = modtypeMatch(env, func2.getArgType(), func1.getArgType());
            if (modMatchResult.isErr()) {
                return modMatchResult.mapErr(Function.identity());
            }
            return modtypeMatch(env.withModule(func2.getArgName(), func2.getArgType()), bodySubst, func2.getBody());
        } else {
            return Result.err(Collections.singletonList(
                    ModuleTypeMismatch.<LOC, NAME, IDENT, VAL, KIND, DEF, TERM>builder()
                            .location(modType1.getLocation())
                            .modType1(modType1)
                            .modType2(modType2)
                            .build()
            ));
        }
    }

    private Result<Void, List<TypeError<LOC>>> specificationMatch(Env<LOC, NAME, IDENT, TERM, VAL, DEF, KIND> env, Subst<LOC, NAME, IDENT> subst, Pair<Specification<LOC, NAME, IDENT, VAL, KIND, DEF>, Specification<LOC, NAME, IDENT, VAL, KIND, DEF>> entry) {
        if (entry.getLeft() instanceof ValueSig) {
            ValueSig<LOC, NAME, IDENT, VAL, KIND, DEF> sig1 = (ValueSig<LOC, NAME, IDENT, VAL, KIND, DEF>)entry.getLeft();
            ValueSig<LOC, NAME, IDENT, VAL, KIND, DEF> sig2 = (ValueSig<LOC, NAME, IDENT, VAL, KIND, DEF>)entry.getRight();
            if (!coreTyping.valtypeMatch(env, sig1.getType(), coreSyntax.substVal(sig2.getType(), subst))) {
                return Result.err(Collections.singletonList(
                        ValueComponentsDoNotMatch.<LOC, VAL>builder()
                                .location(sig2.getName().getLocation())
                                .value1(sig1.getType())
                                .value1(sig2.getType())
                        .build()
                ));
            }
        } else if (entry.getLeft() instanceof TypeSig) {
            TypeSig<LOC, NAME, IDENT, VAL, KIND, DEF> sig1 = (TypeSig<LOC, NAME, IDENT, VAL, KIND, DEF>)entry.getLeft();
            TypeSig<LOC, NAME, IDENT, VAL, KIND, DEF> sig2 = (TypeSig<LOC, NAME, IDENT, VAL, KIND, DEF>)entry.getRight();

            if (!typedeclMatch(env, sig1.getName(), sig1.getDecl(), moduleSyntax.substTypeDecl(sig2.getDecl(), subst))) {
                return Result.err(Collections.singletonList(
                        TypeComponentsDoNotMatch.<LOC, NAME, IDENT, VAL, KIND, DEF>builder()
                                .location(sig2.getName().getLocation())
                                .value1(sig1)
                                .value1(sig2)
                                .build()
                ));
            }
        } else if (entry.getLeft() instanceof ModuleSig) {
            ModuleSig<LOC, NAME, IDENT, VAL, KIND, DEF> sig1 = (ModuleSig<LOC, NAME, IDENT, VAL, KIND, DEF>)entry.getLeft();
            ModuleSig<LOC, NAME, IDENT, VAL, KIND, DEF> sig2 = (ModuleSig<LOC, NAME, IDENT, VAL, KIND, DEF>)entry.getRight();

            Result<Void, List<TypeError<LOC>>> modtypeMatchResult = modtypeMatch(env, sig1.getType(), moduleSyntax.substModuleType(sig2.getType(), subst));
            if (modtypeMatchResult.isErr()) {
                return modtypeMatchResult.mapErr(Function.identity());
            }
        }
        return Result.ok();
    }

    private boolean typedeclMatch(Env<LOC, NAME, IDENT, TERM, VAL, DEF, KIND> env, IDENT id, TypeDecl<KIND, DEF> decl1, TypeDecl<KIND, DEF> decl2) {
        if (coreTyping.kindMatch(env, decl1.getKind(), decl2.getKind())) {
            if (!decl2.getManifest().isPresent()) {
                return true;
            } else if (decl1.getManifest().isPresent() && decl2.getManifest().isPresent()) {
                return coreTyping.deftypeEquiv(env, decl2.getKind(), decl1.getManifest().get(), decl2.getManifest().get());
            } else if (!decl1.getManifest().isPresent() && decl2.getManifest().isPresent()) {
                return coreTyping.deftypeEquiv(env, decl2.getKind(), coreTyping.deftypeOfPath(Path.of(id), decl1.getKind()),  decl2.getManifest().get());
            }
        }
        return false;
    }

    private Result<List<Pair<Specification<LOC, NAME, IDENT, VAL, KIND, DEF>,Specification<LOC, NAME, IDENT, VAL, KIND, DEF>>>, List<TypeError<LOC>>> pairSignatureComponents(
            Signature<LOC, NAME, IDENT, VAL, KIND, DEF> sig1,
            Signature<LOC, NAME, IDENT, VAL, KIND, DEF> sig2
    ) {
        List<Pair<Specification<LOC, NAME, IDENT, VAL, KIND, DEF>,Specification<LOC, NAME, IDENT, VAL, KIND, DEF>>> results = new ArrayList<>();
        for (Specification<LOC, NAME, IDENT, VAL, KIND, DEF> spec1 : sig1.getSpecifications()) {
            Optional<Specification<LOC, NAME, IDENT, VAL, KIND, DEF>> matchingSig = sig2.findMatching(spec1);
            if (!matchingSig.isPresent()) {
                return Result.err(Collections.singletonList(
                    UnmatchedSignatureComponent.<LOC, NAME, IDENT, VAL, KIND, DEF, TERM>builder()
                            .location(spec1.getName().getLocation())
                            .spec(spec1)
                            .sig1(sig1)
                            .sig2(sig2)
                        .build()
                ));
            }
            results.add(Pair.of(spec1, matchingSig.get()));
        }
        return Result.ok(Collections.unmodifiableList(results));
    }

    private ModuleType<LOC, NAME, IDENT, VAL, KIND, DEF> strengthenModtype(Path<LOC, NAME, IDENT> path, ModuleType<LOC, NAME, IDENT, VAL, KIND, DEF> modType) {
        if (modType instanceof Signature) {
            Signature<LOC, NAME, IDENT, VAL, KIND, DEF> sig = (Signature<LOC, NAME, IDENT, VAL, KIND, DEF>)modType;
            PVector<Specification<LOC, NAME, IDENT, VAL, KIND, DEF>> specs = TreePVector.empty();
            for (Specification<LOC, NAME, IDENT, VAL, KIND, DEF> spec : sig.getSpecifications()) {
                specs = specs.plus(strengthenSpec(path, spec));
            }
            return ModuleType.signature(modType.getLocation(), specs);
        } else if (modType instanceof FunctorType) {
            //FunctorType<LOC, NAME, IDENT, VAL, KIND, DEF> func = (FunctorType<LOC, NAME, IDENT, VAL, KIND, DEF>)modType;
            return modType;
        }
        throw new RuntimeException(String.format("Unknown module type (%s): %s", modType.getClass().getCanonicalName(), modType));
    }

    private Specification<LOC, NAME, IDENT, VAL, KIND, DEF> strengthenSpec(Path<LOC, NAME, IDENT> path, Specification<LOC, NAME, IDENT, VAL, KIND, DEF> spec) {
        if (spec instanceof ValueSig) {
            //ValueSig<LOC, NAME, IDENT, VAL, KIND, DEF> sig = (ValueSig<LOC, NAME, IDENT, VAL, KIND, DEF>)spec;
            return spec;
        } else if (spec instanceof TypeSig) {
            TypeSig<LOC, NAME, IDENT, VAL, KIND, DEF> sig = (TypeSig<LOC, NAME, IDENT, VAL, KIND, DEF>)spec;
            Optional<DEF> manifest;
            if (sig.getDecl().getManifest().isPresent()) {
                manifest = sig.getDecl().getManifest();
            } else {
                manifest = Optional.of(coreTyping.deftypeOfPath(path.plus(sig.getName().getName()), sig.getDecl().getKind()));
            }
            return Specification.typeSig(sig.getName(), TypeDecl.of(sig.getDecl().getKind(), manifest));
        } else if (spec instanceof ModuleSig) {
            ModuleSig<LOC, NAME, IDENT, VAL, KIND, DEF> sig = (ModuleSig<LOC, NAME, IDENT, VAL, KIND, DEF>)spec;
            ModuleType<LOC, NAME, IDENT, VAL, KIND, DEF> strengthenedMod = strengthenModtype(path.plus(sig.getName().getName()), sig.getType());
            return Specification.moduleSig(sig.getName(), strengthenedMod);
        }
        throw new RuntimeException(String.format("Unknown specification (%s): %s", spec.getClass().getCanonicalName(), spec));
    }
    
    private Result<Void, List<TypeError<LOC>>> checkModType(Env<LOC, NAME, IDENT, TERM, VAL, DEF, KIND> env, ModuleType<LOC, NAME, IDENT, VAL, KIND, DEF> modType) {
        TypeErrorList<LOC> typeErrors = new TypeErrorList<>();
        if (modType instanceof Signature) {
            Signature<LOC, NAME, IDENT, VAL, KIND, DEF> sig = (Signature<LOC, NAME, IDENT, VAL, KIND, DEF>)modType;
            typeErrors.addResults(checkSignature(env, sig.getSpecifications()));
        } else if (modType instanceof FunctorType) {
            FunctorType<LOC, NAME, IDENT, VAL, KIND, DEF> funcType = (FunctorType<LOC, NAME, IDENT, VAL, KIND, DEF>)modType;
            typeErrors.addResults(checkModType(env, funcType.getArgType()));
            typeErrors.addResults(checkModType(env.withModule(funcType.getArgName(), funcType.getArgType()), funcType.getBody()));
        }
        if (typeErrors.isEmpty()) {
            return Result.ok();
        } else {
            return Result.err(typeErrors);
        }
    }

    private Result<Void, List<TypeError<LOC>>> checkSignature(Env<LOC, NAME, IDENT, TERM, VAL, DEF, KIND> env, Iterable<Specification<LOC, NAME, IDENT, VAL, KIND, DEF>> specs) {
        TypeErrorList<LOC> typeErrors = new TypeErrorList<>();
        SeenIdentifiers<LOC, NAME, IDENT> seen = SeenIdentifiers.empty();
        for (Specification<LOC, NAME, IDENT, VAL, KIND, DEF> spec : specs) {
            if (spec instanceof ValueSig) {
                ValueSig<LOC, NAME, IDENT, VAL, KIND, DEF> sig = (ValueSig<LOC, NAME, IDENT, VAL, KIND, DEF>)spec;
                if (seen.containsValue(sig.getName())) {
                    typeErrors.add(RepeatedValueName.<LOC, NAME, IDENT>builder()
                            .location(sig.getName().getLocation())
                            .original(seen.getValue(sig.getName()).get())
                            .duplicate(sig.getName())
                            .build());
                } else {
                    seen = seen.plusValue(sig.getName());
                }
                typeErrors.addResult(coreTyping.checkValtype(env, sig.getType()));
            } else if (spec instanceof TypeSig) {
                TypeSig<LOC, NAME, IDENT, VAL, KIND, DEF> sig = (TypeSig<LOC, NAME, IDENT, VAL, KIND, DEF>)spec;
                if (seen.containsType(sig.getName())) {
                    typeErrors.add(RepeatedTypeName.<LOC, NAME, IDENT>builder()
                            .location(sig.getName().getLocation())
                            .original(seen.getType(sig.getName()).get())
                            .duplicate(sig.getName())
                            .build());
                } else {
                    seen.plusType(sig.getName());
                }
                typeErrors.addResult(coreTyping.checkKind(env, sig.getDecl().getKind()));
                if (sig.getDecl().getManifest().isPresent()) {
                    KIND kind1 = coreTyping.kindDeftype(env, sig.getDecl().getManifest().get());
                    KIND kind2 = sig.getDecl().getKind();
                    if (!coreTyping.kindMatch(env, kind1, kind2)) {
                        typeErrors.add(KindMismatchInSignature.<LOC, KIND>builder()
                                .location(sig.getName().getLocation())
                                .kind(kind2)
                                .manifestKind(kind1)
                                .build());
                    }
                }
            } else if (spec instanceof ModuleSig) {
                ModuleSig<LOC, NAME, IDENT, VAL, KIND, DEF> sig = (ModuleSig<LOC, NAME, IDENT, VAL, KIND, DEF>)spec;
                if (seen.containsModule(sig.getName())) {
                    typeErrors.add(RepeatedModuleName.<LOC, NAME, IDENT>builder()
                            .location(sig.getName().getLocation())
                            .original(seen.getType(sig.getName()).get())
                            .duplicate(sig.getName())
                            .build());
                } else {
                    seen = seen.plusModule(sig.getName());
                }
                typeErrors.addResults(checkModType(env, sig.getType()));
            }
        }
        if (typeErrors.isEmpty()) {
            return Result.ok();
        } else {
            return Result.err(typeErrors);
        }
    }

    @Value
    private static class SeenIdentifiers<LOC, NAME, IDENT extends Ident<NAME, LOC>> {
        private PMap<NAME, IDENT> values;
        private PMap<NAME, IDENT> modules;
        private PMap<NAME, IDENT> types;

        public static <LOC, NAME, IDENT extends Ident<NAME, LOC>> SeenIdentifiers<LOC, NAME, IDENT> empty() {
            return new SeenIdentifiers<>((PMap<NAME, IDENT>)HashTreePMap.<NAME, IDENT>empty(), (PMap<NAME, IDENT>)HashTreePMap.<NAME, IDENT>empty(), (PMap<NAME, IDENT>)HashTreePMap.<NAME, IDENT>empty());
        }

        private SeenIdentifiers(PMap<NAME, IDENT> values, PMap<NAME, IDENT> modules, PMap<NAME, IDENT> types) {
            this.values = values;
            this.modules = modules;
            this.types = types;
        }

        public boolean containsValue(IDENT ident) {
            return values.containsKey(ident.getName());
        }

        public boolean containsModule(IDENT ident) {
            return modules.containsKey(ident.getName());
        }

        public boolean containsType(IDENT ident) {
            return types.containsKey(ident.getName());
        }

        public Optional<IDENT> getValue(IDENT ident) {
            return Optional.ofNullable(values.get(ident.getName()));
        }

        public Optional<IDENT> getModule(IDENT ident) {
            return Optional.ofNullable(modules.get(ident.getName()));
        }

        public Optional<IDENT> getType(IDENT ident) {
            return Optional.ofNullable(types.get(ident.getName()));
        }

        public SeenIdentifiers<LOC, NAME, IDENT> plusValue(IDENT ident) {
            return new SeenIdentifiers<>(
                    this.values.plus(ident.getName(), ident),
                    this.modules,
                    this.types
            );
        }

        public SeenIdentifiers<LOC, NAME, IDENT> plusModule(IDENT ident) {
            return new SeenIdentifiers<>(
                    this.values,
                    this.modules.plus(ident.getName(), ident),
                    this.types
            );
        }

        public SeenIdentifiers<LOC, NAME, IDENT> plusType(IDENT ident) {
            return new SeenIdentifiers<>(
                    this.values,
                    this.modules,
                    this.types.plus(ident.getName(), ident)
            );
        }
    }

    private static class TypeErrorList<LOC> extends ArrayList<TypeError<LOC>> {
        public <V> void addResult(Result<V, TypeError<LOC>> result) {
            if (result.isErr()) {
                this.add(result.getErr());
            }
        }

        public <V> void addResults(Result<V, List<TypeError<LOC>>> result) {
            if (result.isErr()) {
                this.addAll(result.getErr());
            }
        }
    }
}

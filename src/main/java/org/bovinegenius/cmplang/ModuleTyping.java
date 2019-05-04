package org.bovinegenius.cmplang;

import org.bovinegenius.cmplang.ModuleTerm.LongIdent;
import org.bovinegenius.cmplang.ModuleType.FunctorType;
import org.bovinegenius.cmplang.ModuleType.Signature;
import org.bovinegenius.cmplang.Specification.ModuleSig;
import org.bovinegenius.cmplang.Specification.TypeSig;
import org.bovinegenius.cmplang.Specification.ValueSig;
import org.pcollections.HashTreePSet;
import org.pcollections.PSet;

public class ModuleTyping<NAME, IDENT extends Ident<NAME>, TERM, VAL, DEF, KIND> {
    private final ModuleSyntax<NAME, IDENT, TERM, VAL, DEF, KIND> moduleSyntax;
    private final CoreTyping<NAME, IDENT, TERM, VAL, DEF, KIND> coreTyping;

    private ModuleTyping(ModuleSyntax<NAME, IDENT, TERM, VAL, DEF, KIND> moduleSyntax, CoreTyping<NAME, IDENT, TERM, VAL, DEF, KIND> coreTyping) {
        this.moduleSyntax = moduleSyntax;
        this.coreTyping = coreTyping;
    }

    public ModuleType<NAME, IDENT, VAL, KIND, DEF> typeModule(Env<NAME, IDENT, TERM, VAL, DEF, KIND> env, ModuleTerm<NAME, IDENT, VAL, KIND, DEF, TERM> modTerm) {
        if (modTerm instanceof LongIdent) {
            LongIdent<NAME, IDENT, VAL, KIND, DEF, TERM> ident = (LongIdent<NAME, IDENT, VAL, KIND, DEF, TERM>)modTerm;
            return strengthenModtype(ident.getPath(), env.findModule(ident.getPath()));
        }
    }

    public Specification<NAME, IDENT, VAL, KIND, DEF> typeDefinition(Env<NAME, IDENT, TERM, VAL, DEF, KIND> env, Definition<NAME, IDENT, VAL, KIND, DEF, TERM> definition) {

    }

    
    private void modtypeMatch(Env<NAME, IDENT, TERM, VAL, DEF, KIND> env, ModuleType<NAME, IDENT, VAL, KIND, DEF> modType1, ModuleType<NAME, IDENT, VAL, KIND, DEF> modType2) {

    }

    private ModuleType<NAME, IDENT, VAL, KIND, DEF> strengthenModtype(Path<NAME, IDENT> path, ModuleType<NAME, IDENT, VAL, KIND, DEF> modType) {

    }
    
    private void checkModType(Env<NAME, IDENT, TERM, VAL, DEF, KIND> env, ModuleType<NAME, IDENT, VAL, KIND, DEF> modType) {
        if (modType instanceof Signature) {
            Signature<NAME, IDENT, VAL, KIND, DEF> sig = (Signature<NAME, IDENT, VAL, KIND, DEF>)modType;
            checkSignature(env, HashTreePSet.empty(), HashTreePSet.empty(), HashTreePSet.empty(), sig.getSpecifications());
        } else if (modType instanceof FunctorType) {
            FunctorType<NAME, IDENT, VAL, KIND, DEF> funcType = (FunctorType<NAME, IDENT, VAL, KIND, DEF>)modType;
            checkModType(env, funcType.getArgType());
            checkModType(env.withModule(funcType.getArgName(), funcType.getArgType()), funcType.getBody());
        }
    }
    
    private void checkSignature(Env<NAME, IDENT, TERM, VAL, DEF, KIND> env, PSet<NAME> seenValues, PSet<NAME> seenTypes, PSet<NAME> seenModules, Iterable<Specification<NAME, IDENT, VAL, KIND, DEF>> specs) {
        for (Specification<NAME, IDENT, VAL, KIND, DEF> spec : specs) {
            if (spec instanceof ValueSig) {
                ValueSig<NAME, IDENT, VAL, KIND, DEF> sig = (ValueSig<NAME, IDENT, VAL, KIND, DEF>)spec;
                if (seenValues.contains(sig.getName().getName())) {
                    throw new RuntimeException(String.format("Found duplicate value name %s.", sig.getName().getName()));
                } else {
                    seenValues = seenValues.plus(sig.getName().getName());
                }
                coreTyping.checkValtype(env, sig.getType());
            } else if (spec instanceof TypeSig) {
                TypeSig<NAME, IDENT, VAL, KIND, DEF> sig = (TypeSig<NAME, IDENT, VAL, KIND, DEF>)spec;
                if (seenTypes.contains(sig.getName().getName())) {
                    throw new RuntimeException(String.format("Found duplicate type name %s.", sig.getName().getName()));
                } else {
                    seenTypes = seenTypes.plus(sig.getName().getName());
                }
                coreTyping.checkKind(env, sig.getDecl().getKind());
                if (sig.getDecl().getManifest().isPresent()) {
                    if (!coreTyping.kindMatch(env, coreTyping.kindDeftype(env, sig.getDecl().getManifest().get()), sig.getDecl().getKind())) {
                        throw new RuntimeException("kind mismatch in manifest type specification");
                    }
                }
            } else if (spec instanceof ModuleSig) {
                ModuleSig<NAME, IDENT, VAL, KIND, DEF> sig = (ModuleSig<NAME, IDENT, VAL, KIND, DEF>)spec;
                if (seenModules.contains(sig.getName().getName())) {
                    throw new RuntimeException(String.format("Found duplicate value name %s.", sig.getName().getName()));
                } else {
                    seenModules = seenModules.plus(sig.getName().getName());
                }
                checkModType(env, sig.getType());
            }
        }
    }
}

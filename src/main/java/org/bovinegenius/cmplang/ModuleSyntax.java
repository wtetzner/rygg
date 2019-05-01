package org.bovinegenius.cmplang;

import org.bovinegenius.cmplang.ModuleType.FunctorType;
import org.bovinegenius.cmplang.ModuleType.Signature;
import org.bovinegenius.cmplang.Specification.ModuleSig;
import org.bovinegenius.cmplang.Specification.TypeSig;
import org.bovinegenius.cmplang.Specification.ValueSig;
import org.pcollections.PVector;
import org.pcollections.TreePVector;

public class ModuleSyntax<NAME, IDENT extends Ident<NAME>, TERM, VAL, DEF, KIND> {
    private final CoreSyntax<NAME, IDENT, TERM, VAL, DEF, KIND> coreSyntax;

    private ModuleSyntax(CoreSyntax<NAME, IDENT, TERM, VAL, DEF, KIND> coreSyntax) {
        this.coreSyntax = coreSyntax;
    }

    public CoreSyntax<NAME, IDENT, TERM, VAL, DEF, KIND> getCoreSyntax() {
        return this.coreSyntax;
    }

    public TypeDecl<KIND, DEF> substTypeDecl(TypeDecl<KIND, DEF> typeDecl, Subst<NAME, IDENT> subst) {
        return TypeDecl.of(
                coreSyntax.substKind(typeDecl.getKind(), subst),
                typeDecl.getManifest().map(m -> coreSyntax.substDef(m, subst))
                );
    }

    public ModuleType<NAME, IDENT, VAL, KIND, DEF> substModuleType(ModuleType<NAME, IDENT, VAL, KIND, DEF> moduleType, Subst<NAME, IDENT> subst) {
        if (null == moduleType) {
            return null;
        }
        if (moduleType instanceof Signature) {
            Signature<NAME, IDENT, VAL, KIND, DEF> sig = (Signature<NAME, IDENT, VAL, KIND, DEF>)moduleType;
            PVector<Specification<NAME, IDENT, VAL, KIND, DEF>> newSpecs = TreePVector.empty();
            for (Specification<NAME, IDENT, VAL, KIND, DEF> spec : sig.getSpecifications()) {
                newSpecs = newSpecs.plus(substSigItem(spec, subst));
            }
            return ModuleType.signature(newSpecs);
        } else if (moduleType instanceof FunctorType) {
            FunctorType<NAME, IDENT, VAL, KIND, DEF> func = (FunctorType<NAME, IDENT, VAL, KIND, DEF>)moduleType;
            return ModuleType.functorType(func.getArgName(), substModuleType(func.getArgType(), subst), substModuleType(func.getBody(), subst));
        } else {
            throw new IllegalArgumentException(String.format("Unknown module type (%s): %s", moduleType.getClass().getCanonicalName(), moduleType));
        }
    }

    private Specification<NAME, IDENT, VAL, KIND, DEF> substSigItem(Specification<NAME, IDENT, VAL, KIND, DEF> spec, Subst<NAME, IDENT> subst) {
        if (null == spec) {
            return null;
        }
        if (spec instanceof ValueSig) {
            ValueSig<NAME, IDENT, VAL, KIND, DEF> sig = (ValueSig<NAME, IDENT, VAL, KIND, DEF>)spec;
            return Specification.valueSig(sig.getName(), coreSyntax.substVal(sig.getType(), subst));
        } else if (spec instanceof TypeSig) {
            TypeSig<NAME, IDENT, VAL, KIND, DEF> sig = (TypeSig<NAME, IDENT, VAL, KIND, DEF>)spec;
            return Specification.typeSig(sig.getName(), substTypeDecl(sig.getDecl(), subst));
        } else if (spec instanceof ModuleSig) {
            ModuleSig<NAME, IDENT, VAL, KIND, DEF> sig = (ModuleSig<NAME, IDENT, VAL, KIND, DEF>)spec;
            return Specification.moduleSig(sig.getName(), substModuleType(sig.getType(), subst));
        } else {
            throw new IllegalArgumentException(String.format("Unknown sig item type (%s): %s", spec.getClass().getCanonicalName(), spec));
        }
    }
}

package org.bovinegenius.cmplang.ast.module;

import org.bovinegenius.cmplang.ast.module.ModuleType.FunctorType;
import org.bovinegenius.cmplang.ast.module.ModuleType.Signature;
import org.bovinegenius.cmplang.ast.module.Specification.ModuleSig;
import org.bovinegenius.cmplang.ast.module.Specification.TypeSig;
import org.bovinegenius.cmplang.ast.module.Specification.ValueSig;
import org.pcollections.PVector;
import org.pcollections.TreePVector;

public class ModuleSyntax<LOC, NAME, IDENT extends Ident<NAME, LOC>, TERM, VAL, DEF, KIND> {
    private final CoreSyntax<LOC, NAME, IDENT, TERM, VAL, DEF, KIND> coreSyntax;

    private ModuleSyntax(CoreSyntax<LOC, NAME, IDENT, TERM, VAL, DEF, KIND> coreSyntax) {
        this.coreSyntax = coreSyntax;
    }

    public CoreSyntax<LOC, NAME, IDENT, TERM, VAL, DEF, KIND> getCoreSyntax() {
        return this.coreSyntax;
    }

    public TypeDecl<KIND, DEF> substTypeDecl(TypeDecl<KIND, DEF> typeDecl, Subst<LOC, NAME, IDENT> subst) {
        return TypeDecl.of(
                coreSyntax.substKind(typeDecl.getKind(), subst),
                typeDecl.getManifest().map(m -> coreSyntax.substDef(m, subst))
        );
    }

    public ModuleType<LOC, NAME, IDENT, VAL, KIND, DEF> substModuleType(ModuleType<LOC, NAME, IDENT, VAL, KIND, DEF> moduleType, Subst<LOC, NAME, IDENT> subst) {
        if (null == moduleType) {
            return null;
        }
        if (moduleType instanceof Signature) {
            Signature<LOC, NAME, IDENT, VAL, KIND, DEF> sig = (Signature<LOC, NAME, IDENT, VAL, KIND, DEF>)moduleType;
            PVector<Specification<LOC, NAME, IDENT, VAL, KIND, DEF>> newSpecs = TreePVector.empty();
            for (Specification<LOC, NAME, IDENT, VAL, KIND, DEF> spec : sig.getSpecifications()) {
                newSpecs = newSpecs.plus(substSigItem(spec, subst));
            }
            return ModuleType.signature(sig.getLocation(), newSpecs);
        } else if (moduleType instanceof FunctorType) {
            FunctorType<LOC, NAME, IDENT, VAL, KIND, DEF> func = (FunctorType<LOC, NAME, IDENT, VAL, KIND, DEF>)moduleType;
            return ModuleType.functorType(func.getLocation(), func.getArgName(), substModuleType(func.getArgType(), subst), substModuleType(func.getBody(), subst));
        } else {
            throw new IllegalArgumentException(String.format("Unknown module type (%s): %s", moduleType.getClass().getCanonicalName(), moduleType));
        }
    }

    private Specification<LOC, NAME, IDENT, VAL, KIND, DEF> substSigItem(Specification<LOC, NAME, IDENT, VAL, KIND, DEF> spec, Subst<LOC, NAME, IDENT> subst) {
        if (null == spec) {
            return null;
        }
        if (spec instanceof ValueSig) {
            ValueSig<LOC, NAME, IDENT, VAL, KIND, DEF> sig = (ValueSig<LOC, NAME, IDENT, VAL, KIND, DEF>)spec;
            return Specification.valueSig(sig.getName(), coreSyntax.substVal(sig.getType(), subst));
        } else if (spec instanceof TypeSig) {
            TypeSig<LOC, NAME, IDENT, VAL, KIND, DEF> sig = (TypeSig<LOC, NAME, IDENT, VAL, KIND, DEF>)spec;
            return Specification.typeSig(sig.getName(), substTypeDecl(sig.getDecl(), subst));
        } else if (spec instanceof ModuleSig) {
            ModuleSig<LOC, NAME, IDENT, VAL, KIND, DEF> sig = (ModuleSig<LOC, NAME, IDENT, VAL, KIND, DEF>)spec;
            return Specification.moduleSig(sig.getName(), substModuleType(sig.getType(), subst));
        } else {
            throw new IllegalArgumentException(String.format("Unknown sig item type (%s): %s", spec.getClass().getCanonicalName(), spec));
        }
    }
}

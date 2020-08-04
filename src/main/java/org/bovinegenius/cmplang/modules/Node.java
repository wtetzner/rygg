package org.bovinegenius.cmplang.modules;

import org.bovinegenius.cmplang.ast.module.HasLocation;
import org.bovinegenius.cmplang.util.Formatted;

public interface Node<LOC extends Comparable<LOC>, NAME, SELF> extends Substable<LOC, NAME, SELF>, HasLocation<LOC>, Formatted {

}

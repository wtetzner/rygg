package org.bovinegenius.cmplang.ast.module;

import java.util.List;

public interface Source {
    List<String> geteLines(String sourceText);
}

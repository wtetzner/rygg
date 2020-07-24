package org.bovinegenius.cmplang.util;

import java.util.Optional;

public interface Formatted {
    String formatted(boolean inline, int indentAmount, int indentLevel);

    default String formatted(int indentAmount, int indentLevel) {
        return this.formatted(true, indentAmount, indentLevel);
    }

    default String formatted(int indentAmount) {
        return this.formatted(indentAmount, 0);
    }

    default String formatted() {
        return this.formatted(4);
    }

    static String indent(int indentAmount, int indentLevel) {
        return spaces(indentAmount *  indentLevel);
    }

    static String spaces(int indentAmount) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < indentAmount; i++) {
            sb.append(" ");
        }
        return sb.toString();
    }

    static String formatFirst(boolean inline, int indentAmount, int indentLevel, Optional<? extends Formatted>... formatted) {
        for (Optional<? extends Formatted> formattedValue : formatted) {
            if(formattedValue.isPresent()) {
                return formattedValue.get().formatted(inline, indentAmount, indentLevel);
            }
        }
        throw new RuntimeException("No value found");
    }

}

package com.rojoma.json.v3.util.codecs.time;

import java.time.*;
import java.time.format.DateTimeFormatter;

// This exists for scala 2.10 and 2.11; scala 2.12 and 2.13 know how
// to pass a method handle to Java, but the earlier ones do not.
final class ParseHelper {
    private ParseHelper() {}

    static Instant parseInstant(String s) {
        return DateTimeFormatter.ISO_OFFSET_DATE_TIME.parse(s, Instant::from);
    }

    static OffsetDateTime parseOffsetDateTime(String s) {
        return DateTimeFormatter.ISO_OFFSET_DATE_TIME.parse(s, OffsetDateTime::from);
    }

    static OffsetTime parseOffsetTime(String s) {
        return DateTimeFormatter.ISO_OFFSET_TIME.parse(s, OffsetTime::from);
    }

    static LocalDateTime parseLocalDateTime(String s) {
        return DateTimeFormatter.ISO_LOCAL_DATE_TIME.parse(s, LocalDateTime::from);
    }

    static LocalDate parseLocalDate(String s) {
        return DateTimeFormatter.ISO_LOCAL_DATE.parse(s, LocalDate::from);
    }

    static LocalTime parseLocalTime(String s) {
        return DateTimeFormatter.ISO_LOCAL_TIME.parse(s, LocalTime::from);
    }

    static Instant parseInstantRFC1123(String s) {
        return DateTimeFormatter.RFC_1123_DATE_TIME.parse(s, Instant::from);
    }

    static OffsetDateTime parseOffsetDateTimeRFC1123(String s) {
        return DateTimeFormatter.RFC_1123_DATE_TIME.parse(s, OffsetDateTime::from);
    }
}

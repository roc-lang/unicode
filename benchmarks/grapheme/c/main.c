#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include <grapheme.h>

static unsigned char *
read_source(size_t *source_len, uint64_t *repeats)
{
	unsigned char header[9];
	unsigned char *source = NULL;
	size_t capacity = 0;

	if (fread(header, 1, sizeof(header), stdin) != sizeof(header) ||
	    header[8] != '\n') {
		fputs("expected 8-digit repeat count, newline, and UTF-8 corpus\n",
		      stderr);
		exit(1);
	}
	*repeats = 0;
	for (size_t i = 0; i < 8; i++) {
		if (header[i] < '0' || header[i] > '9') {
			fputs("invalid repeat count\n", stderr);
			exit(1);
		}
		*repeats = *repeats * 10 + (uint64_t)(header[i] - '0');
	}

	*source_len = 0;
	for (;;) {
		if (*source_len == capacity) {
			capacity = capacity == 0 ? 4096 : capacity * 2;
			unsigned char *grown = realloc(source, capacity);
			if (grown == NULL) {
				perror("realloc");
				free(source);
				exit(1);
			}
			source = grown;
		}
		size_t read = fread(source + *source_len, 1,
		                    capacity - *source_len, stdin);
		*source_len += read;
		if (read == 0) {
			if (ferror(stdin)) {
				perror("fread");
				free(source);
				exit(1);
			}
			break;
		}
	}
	return source;
}

static size_t
next_break(const unsigned char *source, size_t source_len, size_t offset)
{
	size_t width = grapheme_next_character_break_utf8(
		(const char *)source + offset, source_len - offset);
	if (width == 0 || width > source_len - offset) {
		fputs("libgrapheme returned an invalid break offset\n", stderr);
		exit(1);
	}
	return width;
}

int
main(void)
{
	size_t source_len;
	uint64_t repeats;
	unsigned char *source = read_source(&source_len, &repeats);

	if (repeats == 0) {
		uint64_t count = 0;
		uint64_t sum_ends = 0;
		uint64_t weighted_ends = 0;
		for (size_t offset = 0; offset < source_len;) {
			offset += next_break(source, source_len, offset);
			count++;
			sum_ends += (uint64_t)offset;
			weighted_ends += count * (uint64_t)offset;
		}
		printf("%" PRIu64 " %" PRIu64 " %" PRIu64 "\n", count,
		       sum_ends, weighted_ends);
		free(source);
		return 0;
	}

	uint64_t total = 0;
	for (uint64_t i = 0; i < repeats; i++) {
		for (size_t offset = 0; offset < source_len;) {
			offset += next_break(source, source_len, offset);
			total++;
		}
	}
	printf("%" PRIu64 "\n", total);
	free(source);
	return 0;
}

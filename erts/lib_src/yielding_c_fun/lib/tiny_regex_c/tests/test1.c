/*
 * Testing various regex-patterns
 */

#include <stdio.h>
#include <string.h>
#include "re.h"


#define OK    ((char*) 1)
#define NOK   ((char*) 0)


char* test_vector[][3] =
{
  { OK,  "\\d",                       "5"                },
  { OK,  "\\w+",                      "hej"              },
  { OK,  "\\s",                       "\t \n"            },
  { NOK, "\\S",                       "\t \n"            },
  { OK,  "[\\s]",                     "\t \n"            },
  { NOK, "[\\S]",                     "\t \n"            },
  { NOK, "\\D",                       "5"                },
  { NOK, "\\W+",                      "hej"              },
  { OK,  "[0-9]+",                    "12345"            },
  { OK,  "\\D",                       "hej"              },
  { NOK, "\\d",                       "hej"              },
  { OK,  "[^\\w]",                    "\\"               },
  { OK,  "[\\W]",                     "\\"               },
  { NOK, "[\\w]",                     "\\"               },
  { OK,  "[^\\d]",                    "d"                },
  { NOK, "[\\d]",                     "d"                },
  { NOK, "[^\\D]",                    "d"                },
  { OK,  "[\\D]",                     "d"                },
  { OK,  "^.*\\\\.*$",                "c:\\Tools"        },
  { OK,  "^[\\+-]*[\\d]+$",           "+27"              },
  { OK,  "[abc]",                     "1c2"              },
  { NOK, "[abc]",                     "1C2"              },
  { OK,  "[1-5]+",                    "0123456789"       },
  { OK,  "[.2]",                      "1C2"              },
  { OK,  "a*$",                       "Xaa"              },
  { OK,  "a*$",                       "Xaa"              },
  { OK,  "[a-h]+",                    "abcdefghxxx"      },
  { NOK, "[a-h]+",                    "ABCDEFGH"         },
  { OK,  "[A-H]+",                    "ABCDEFGH"         },
  { NOK, "[A-H]+",                    "abcdefgh"         },
  { OK,  "[^\\s]+",                   "abc def"          },
  { OK,  "[^fc]+",                    "abc def"          },
  { OK,  "[^d\\sf]+",                 "abc def"          },
  { OK,  "\n",                        "abc\ndef"         },
  { OK,  "b.\\s*\n",                  "aa\r\nbb\r\ncc\r\n\r\n" },
  { OK,  ".*c",                       "abcabc"           },
  { OK,  ".+c",                       "abcabc"           },
  { OK,  "[b-z].*",                   "ab"               },
  { OK,  "b[k-z]*",                   "ab"               },
  { NOK, "[0-9]",                     "  - "             },
  { OK,  "[^0-9]",                    "  - "             },
  { OK,  "0|",                        "0|"               },
  { NOK, "\\d\\d:\\d\\d:\\d\\d",      "0s:00:00"         },
  { NOK, "\\d\\d:\\d\\d:\\d\\d",      "000:00"           },
  { NOK, "\\d\\d:\\d\\d:\\d\\d",      "00:0000"          },
  { NOK, "\\d\\d:\\d\\d:\\d\\d",      "100:0:00"         },
  { NOK, "\\d\\d:\\d\\d:\\d\\d",      "00:100:00"        },
  { NOK, "\\d\\d:\\d\\d:\\d\\d",      "0:00:100"         },
  { OK,  "\\d\\d?:\\d\\d?:\\d\\d?",   "0:0:0"            },
  { OK,  "\\d\\d?:\\d\\d?:\\d\\d?",   "0:00:0"           },
  { OK,  "\\d\\d?:\\d\\d?:\\d\\d?",   "0:0:00"           },
  { OK,  "\\d\\d?:\\d\\d?:\\d\\d?",   "00:0:0"           },
  { OK,  "\\d\\d?:\\d\\d?:\\d\\d?",   "00:00:0"          },
  { OK,  "\\d\\d?:\\d\\d?:\\d\\d?",   "00:0:00"          },
  { OK,  "\\d\\d?:\\d\\d?:\\d\\d?",   "0:00:00"          },
  { OK,  "\\d\\d?:\\d\\d?:\\d\\d?",   "00:00:00"         },
  { OK,  "[Hh]ello [Ww]orld\\s*[!]?", "Hello world !"    },
  { OK,  "[Hh]ello [Ww]orld\\s*[!]?", "hello world !"    },
  { OK,  "[Hh]ello [Ww]orld\\s*[!]?", "Hello World !"    },
  { OK,  "[Hh]ello [Ww]orld\\s*[!]?", "Hello world!   "  },
  { OK,  "[Hh]ello [Ww]orld\\s*[!]?", "Hello world  !"   },
  { OK,  "[Hh]ello [Ww]orld\\s*[!]?", "hello World    !" },
  { NOK, "\\d\\d?:\\d\\d?:\\d\\d?",   "a:0"              }, /* Failing test case reported in https://github.com/kokke/tiny-regex-c/issues/12 */
/*
  { OK,  "[^\\w][^-1-4]",     ")T"          },
  { OK,  "[^\\w][^-1-4]",     ")^"          },
  { OK,  "[^\\w][^-1-4]",     "*)"          },
  { OK,  "[^\\w][^-1-4]",     "!."          },
  { OK,  "[^\\w][^-1-4]",     " x"          },
  { OK,  "[^\\w][^-1-4]",     "$b"          },
*/
  { OK,  ".?bar",                      "real_bar"        },
  { NOK, ".?bar",                      "real_foo"        },
  { NOK, "X?Y",                        "Z"               },
};


void re_print(re_t);

int main()
{
    char* text;
    char* pattern;
    int should_fail;
    size_t ntests = sizeof(test_vector) / sizeof(*test_vector);
    size_t nfailed = 0;
    size_t i;

    for (i = 0; i < ntests; ++i)
    {
        pattern = test_vector[i][1];
        text = test_vector[i][2];
        should_fail = (test_vector[i][0] == NOK);

        int m = re_match(pattern, text);

        if (should_fail)
        {
            if (m != (-1))
            {
                printf("\n");
                re_print(re_compile(pattern));
                fprintf(stderr, "[%lu/%lu]: pattern '%s' matched '%s' unexpectedly. \n", (i+1), ntests, pattern, text);
                nfailed += 1;
            }
        }
        else
        {
            if (m == (-1))
            {
                printf("\n");
                re_print(re_compile(pattern));
                fprintf(stderr, "[%lu/%lu]: pattern '%s' didn't match '%s' as expected. \n", (i+1), ntests, pattern, text);
                nfailed += 1;
            }
        }
    }

    // printf("\n");
    printf("%lu/%lu tests succeeded.\n", ntests - nfailed, ntests);
    printf("\n");
    printf("\n");
    printf("\n");

    return 0;
}

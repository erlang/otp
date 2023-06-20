# CT test_server

## Problem (GH-7119, OTP-11894, OTP-14480)
I think the most confusing thing is that today OTP behavior and design seems to be a mix of Configuration and Testcase centric attributes:
1. (Configuration centric) CT hook callback looks as designed to wrap around CT Configuration functions (i.e. you have *pre* and *post* to wrapp around init_per_testcase or end_per_testcase)
   - Furthermore if you consider hook callback function names, there are no hooks wrapping around Testcase function at all!
2. (Testcase centric) AND at the same the hook execution order is determined by relation to CT Testcase callback
### Next step ideas
1. improve existing documentation for hooks (actually it was planned for many years but down prioritized)
2. add mermaid diagrams to docs when that is possible
3. introduce a CT option for Configuration centric hook execution order (maybe named ct_hooks_order := [testcase(default) | configuration])
### CT hooks priorities (documentation sketch)
Let's assume:
1. cth_A and cth_B being CT hook modules to be installed
2. A, B are CT hook priorities where A is more important than B (higher priority)
3. hook priorities affect order in which hook callbacks of the same type are executed
4. Testcase functions - are CT callback functions defined in suite which will have test verdict assigned (e.g. ok, fail, skip)
5. Configuration functions - are CT callback functions defining initialization and cleanup routines for testcases (e.g. init_per_testcase, end_per_testcase)

```mermaid
---
title: Diagram legend
---
flowchart LR
    dc((("double circle"))) -.- i>item determining hook execution order]
    pgr[/"parallelogram"/] -.- j>item not relevant for hook execution order]
```

#### Testcase centric (default)
1. execution order is related to position in relation to Testcase
2. all *init* hook callbacks(positioned before Testcase) are executed in some order 
3. all *end* hook callbacks(positioned after Testcase) are executed in reversed order
4. *pre* and *post* hook callback type is not affecting execution order
```mermaid
---
title: Testcase centric CT hook execution order (default)
---
flowchart TD
    subgraph hooks
    pre_init_pt_A["(A) pre_init_per_testcase"] --> pre_init_pt_B
    end
    subgraph suite
    pre_init_pt_B["(B) pre_init_per_testcase"] --> init_pt[/"init_per_testcase"/]
    end
    init_pt --> post_init_pt_A
    subgraph hooks
    post_init_pt_A["(A) post_init_per_testcase"] --> post_init_pt_B
    end
    subgraph suite
    post_init_pt_B["(B) post_init_per_testcase"] --> testcase
    testcase((("Testcase")))
    end
    subgraph hooks
    testcase --> pre_end_pt_B
    pre_end_pt_B["(B) pre_end_per_testcase"] --> pre_end_pt_A
    end
    subgraph suite
    pre_end_pt_A["(A) pre_end_per_testcase"] --> end_per_test_case
    end
    subgraph hooks
    end_per_test_case[/"end_per_testcase"/] --> post_end_pt_B
    post_end_pt_B["(B) post_end_per_testcase"] --> post_end_pt_A["(A) post_end_per_testcase"]
    end
```
#### Configuration centric (option candidate)
- all *pre* hooks will be executed in some order
- all *post* hooks will be executed in reversed order
- relation to Testcase is not relevant for hook execution order
- Note: I think, it might be considered a bit inconsistent because there are no hook callbacks wrapping around Testcase (e.g. pre_testcase, post_testcase)
- Above could be ugly workarounded by using pre_init_per_testcase with post_end_per_testcase
- Adding pre_testcase, post_testcase might not be simple but maybe it is not needed

```mermaid
---
title: Configuration centric CT hook execution order (option)
---
flowchart TD
    subgraph hooks
    pre_init_pt_A["(A) pre_init_per_testcase"] --> pre_init_pt_B
    end
    subgraph suite
    pre_init_pt_B["(B) pre_init_per_testcase"] --> init_pt((("init_per_testcase")))
    end
    init_pt --> post_init_pt_B
    subgraph hooks
    post_init_pt_B["(B) post_init_per_testcase"] --> post_init_pt_A
    end
    subgraph suite
    post_init_pt_A["(A) post_init_per_testcase"] --> testcase
    testcase[/"Testcase"/]
    end
    subgraph hooks
    testcase --> pre_end_pt_A
    pre_end_pt_A["(A) pre_end_per_testcase"] --> pre_end_pt_B
    end
    subgraph suite
    pre_end_pt_B["(B) pre_end_per_testcase"] --> end_per_test_case
    end
    subgraph hooks
    end_per_test_case((("end_per_testcase"))) --> post_end_pt_B
    post_end_pt_B["(B) post_end_per_testcase"] --> post_end_pt_A["(A) post_end_per_testcase"]
    end
```

## processes
```mermaid
flowchart LR
    tc["`tc
         every CT callback function`"]
    testcase["`testcase
               the user Testcase`"]
    testcase --> tc
```

## test_server code

```mermaid
mindmap
    code
        run_test_case_apply/1
            run_test_case_apply/6
                ))run_test_case_eval_fun/9((
                    run_test_case_eval/9
                        do_init_tc_call/4
                        run_test_case_eval1/6
                        do_end_tc_call/4
                ct_util mark_process/0
                run_test_case_msgloop/1
                    [receive]
        run_test_case_eval1/6
            ts_tc/3
```
## processes
```mermaid
flowchart LR
    job["`(job process GL)
        captures output`"]
    case["`(case process)
        runs test suite`"]

    case --news--> job
```
## concept
```mermaid
mindmap
  root)test_server(
    interfaces
        sup
            get_loc/1
            set_ct_state/1
        ctrl
            run_test_case_apply/1
            init_target_info/0
            init_memory_checker/0
        suite
    init
        target_info
        memory_checker
    questions
        why call_end_conf not called when init/end_per_testcase not defined?
```

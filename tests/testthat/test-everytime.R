context("pick_urls")
test_results <- function() {
    ## URLs and (complicated) URL lists for testing
    urls <- c("http://www.example.com/",
              "ftp://cran.r-project.org",
              "HtTp://127.0.0.1/foo.php?id=1,2,3",
              "http://en.wikipedia.org/wiki/R_(programming_language)",
              "https://a,b,c@[vf.a1,b2]/foo,bar")
    items2 <- c(urls,
                paste0("\\url{", urls, "}"),
                paste0("\\href{", urls, "}{A description}"),
                paste0("\"", urls, "\""),
                paste0("<", urls, ">"),
                paste0("><", urls, ">"),
                paste0("<", urls, ">>"),
                paste0("<<", urls, ">>"),
                paste0("<", urls, "><"),
                paste0("<URL:", urls, ">"),
                paste0("<URL: \n", urls, " >"),
                paste0("URL:", urls),
                paste0("url:URL:Url:", urls),
                paste0("\\", urls, "\\"),
                paste0("|", urls, "|"),
                paste0("/", urls, "/"),
                paste0("(", urls, ")"),
                paste0("[", urls, "]"),
                paste0("{", urls, "}"),
                ## Nested brackets (opening bracket before each URL)
                paste0("(", paste0(urls, collapse = ", ("),
                       paste0(rep(")", length(urls)), collapse = "")),
                paste0("[", paste0(urls, collapse = ", ["),
                       paste0(rep("]", length(urls)), collapse = "")),
                paste0("{", paste0(urls, collapse = ", {"),
                       paste0(rep("}", length(urls)), collapse = "")),
                ## Nested brackets (closing bracket after each URL)
                paste0(paste0(rep("(", length(urls)), collapse = ""),
                       paste0(urls, collapse = "), "), ")"),
                paste0(paste0(rep("[", length(urls)), collapse = ""),
                       paste0(urls, collapse = "], "), "]"),
                paste0(paste0(rep("{", length(urls)), collapse = ""),
                       paste0(urls, collapse = "}, "), "}"))
    items3 <- c(items2,
                paste0(items2, collapse = " "),
                paste0(items2, collapse = ","),
                paste0(items2, collapse = ", \n"),
                paste0("A complicated URL list follows: ",
                       paste0(items2, collapse = " and "),
                       " and that was it!"))

    norm_urls <- sub("^HtTp", "http", urls)

    pu1_combi <- pick_urls(items3)
    test_that("pick_urls works with default arguments", {
        expect_identical(pu1_combi, rep(norm_urls, 25 * 5))
    })

    ## Multiple strings in one call vs. one string in each of multiple
    ## calls. Note that if some URLs are delimited with angle brackets or
    ## double quotes while others are not, the order or URLs returned may
    ## be different from the order in which they appear in the input (of
    ## course, depending on the input). In this case the order is not
    ## changed.
    pu1_individual <- unlist(lapply(items3, pick_urls))
    test_that("single vs multiple inputs works as expected", {
        expect_identical(pu1_combi, pu1_individual)
    })

    good_host <- c("www", "www2", "ftp", "mail", "news", "smtp",
                   "pop", "pop3", "imap")
    bad_host <- paste0(sample(c(setdiff(letters, "w"), setdiff(LETTERS, "W"),
                                as.character(0:9)), length(good_host)),
                       good_host)
    heur_good_in <- paste0("http://www.example.org/1,",
                           good_host, ".example.org")
    heur_good_target <- as.vector(rbind("http://www.example.org/1",
                                        paste0(good_host, ".example.org")))
    heur_bad_in <- paste0("http://www.example.org/1,",
                          bad_host, ".example.org")
    n_in <- length(good_host)
    pu_goodhost1 <- pick_urls(heur_good_in)
    pu_goodhost2 <- pick_urls(heur_good_in, url_pattern="", need_scheme=FALSE)
    pu_badhost <- pick_urls(heur_bad_in)
    test_that("heuristic splitting uses the hostname as a criterion", {
        expect_identical(pu_goodhost1, rep("http://www.example.org/1", n_in))
        expect_identical(pu_goodhost2, heur_good_target)
        expect_identical(pu_badhost, heur_bad_in)
    })

    ## Demonstrate different behavior when single_item=TRUE

    url_rep <- paste0(rep(urls[3], 2), collapse=",")
    pu2_combi <- pick_urls(url_rep)
    pu3_combi <- pick_urls(url_rep, single_item=TRUE)
    test_that("heuristic splitting works as expected", {
        expect_identical(pu2_combi,
                         rep(sub("^HtTp", "http", urls[3]), 2))
        expect_identical(pu3_combi,
                         sub("^HtTp", "http", url_rep))
    })
    pu2_individual <- unlist(lapply(url_rep, pick_urls))
    pu3_individual <- unlist(lapply(url_rep, pick_urls, single_item=TRUE))
    test_that("heuristics also work for individual inputs", {
        expect_identical(pu2_combi, pu2_individual)
        expect_identical(pu3_combi, pu3_individual)
    })
    pu4_combi <- pick_urls(items2, single_item=TRUE)
    sub_items <- items3[-seq_along(items2)]
    pu5_combi <- pick_urls(sub_items, single_item=TRUE)
    test_that("single_item = TRUE works, multiple inputs", {
        expect_identical(pu4_combi, c(rep(norm_urls, 19), rep(urls[1], 6)))
        expect_identical(pu5_combi, rep(urls[1], 4))
    })
    pu4_individual <- unlist(lapply(items2, pick_urls, single_item=TRUE))
    pu5_individual <- unlist(lapply(sub_items, pick_urls, single_item=TRUE))
    test_that("single_item = TRUE works, single inputs", {
        expect_identical(pu4_combi, pu4_individual)
        expect_identical(pu5_combi, pu5_individual)
    })

    ## Other tests

    end_punct <- c(".", "!", "?")
    url_phrases <-
        paste0(c("See ", "Take a look at ", "Did you visit "),
               paste0(c(urls[-length(urls)],
                        paste0("and ", urls[length(urls)])), collapse = ", "),
               end_punct)
    pu6_combi <- pick_urls(url_phrases)
    pu7_combi <- pick_urls(url_phrases, rm_endpunct=FALSE)
    test_that("end punctuation is removed", {
        expect_identical(pu6_combi, rep(norm_urls, length(end_punct)))
        expect_identical(pu7_combi,
                         as.vector(rbind(matrix(norm_urls[-length(norm_urls)],
                                                length(norm_urls) - 1,
                                                length(end_punct)),
                                         paste0(norm_urls[length(norm_urls)],
                                                end_punct))))
    })
    long_punct <- c("Testing", "a", "sentence", "split", "on", "many",
                    "lines", "/", "items", "with", "final", "item",
                    paste0(urls[4], "."))
    n_lp <- length(long_punct)
    pu8a <- pick_urls(long_punct, rm_endpunct = n_lp)
    pu8b <- pick_urls(long_punct, rm_endpunct = n_lp - 1)
    pu8c <- pick_urls(long_punct, rm_endpunct = TRUE)
    pu8d <- pick_urls(long_punct, rm_endpunct = FALSE)
    long_punct2 <- c(long_punct[-n_lp],
                     paste(long_punct[n_lp], long_punct[1]),
                     long_punct[-1])
    pu9a <- pick_urls(long_punct2, rm_endpunct = n_lp)
    pu9b <- pick_urls(long_punct2, rm_endpunct = n_lp - 1)
    pu9c <- pick_urls(long_punct2, rm_endpunct = TRUE)
    pu9d <- pick_urls(long_punct2, rm_endpunct = FALSE)
    long_punct3 <- sub("items", "items .", long_punct, fixed = TRUE)
    pu10 <- pick_urls(long_punct3, rm_endpunct = TRUE)
    test_that("removal of end punctuation works across items", {
        expect_identical(pu8a, urls[4])
        expect_identical(pu8b, paste0(urls[4], "."))
        expect_identical(pu8c, pu8a)
        expect_identical(pu8d, pu8b)
        expect_identical(pu9a, rep(urls[4], 2))
        expect_equal(length(pu9b), 2)
        expect_identical(pu9b[1], paste0(urls[4], "."))
        expect_true(pu9b[2] %in% c(urls[4], paste0(urls[4], ".")))
        expect_identical(pu9c, pu9a)
        expect_identical(pu9d, rep(paste0(urls[4], "."), 2))
        expect_identical(pu10, paste0(urls[4], "."))
    })

    bad_start1 <- paste0("<mailto:#><", urls[1], ">\"", urls[2], "\"")
    bad_start2 <- paste0("<mailto:#>\"", urls[1], "\"<", urls[2], ">")
    bad_start3 <- paste0("<URL:,a><", urls[1], ">\"", urls[2], "\"")
    bad_start4 <- paste0("<URL:,a>\"", urls[1], "\"<", urls[2], ">")
    pu11a <- pick_urls(bad_start1, single_item = TRUE)
    pu11b <- pick_urls(bad_start2, single_item = TRUE)
    pu12a <- pick_urls(bad_start3, single_item = TRUE)
    pu12b <- pick_urls(bad_start4, single_item = TRUE)
    test_that("bad URL does not break things when single_item is TRUE", {
        expect_identical(pu11a, urls[1])
        expect_identical(pu11b, urls[1])
        expect_identical(pu12a, urls[1])
        expect_identical(pu12b, urls[1])
    })

    bad_auth <- paste0("http://", c("!", "]", "`"))
    pu13 <- pick_urls(bad_auth, url_pattern="")
    test_that("authority part is checked", {
        expect_identical(pu13, rep("http://", 3))
    })

    pu14 <- pick_urls(paste0("<", urls[2], ",", urls[2], ">"))
    test_that("angle brackets don't prevent splitting", {
        expect_identical(pu14, rep(urls[2], 2))
    })

    only_url <- c("URL:", "Url:", "url:", "Url:url:URL:", "url:URL:")
    only_url <- c(only_url, paste0("<", only_url, ">"))
    pu15a <- pick_urls(only_url, url_pattern="", need_scheme=FALSE)
    pu15b <- pick_urls(only_url, url_pattern="")
    pu15c <- pick_urls(only_url, url_pattern="", single_item=TRUE,
                       need_scheme=FALSE)
    pu15d <- pick_urls(only_url, url_pattern="", single_item=TRUE)
    test_that("URL needs more than (repeated) \"URL:\"", {
        expect_equal(length(pu15a), 0)
        expect_equal(length(pu15b), 0)
        expect_equal(length(pu15c), 0)
        expect_equal(length(pu15d), 0)
    })

    all_chars <- paste0("vwxyz://user@[::1]:6453/~abcdfghi/jklm-nop.qt_",
                        "789DEFG+HIJKLMNOPQRSTUX(Y%20Z)?A=B!,$&'*;C#VW")
    pu_all <- pick_urls(all_chars, url_pattern="")
    test_that("all legal characters in the ASCII range are accepted", {
        expect_identical(pu_all, all_chars)
    })

    double_uinfo1 <- "ftp://user@user@example.org"
    double_uinfo2 <- paste0(double_uinfo1, ",http://www.example.org/")
    pu_uinfo1 <- pick_urls(double_uinfo1, url_pattern="", need_scheme=FALSE)
    pu_uinfo2 <- pick_urls(double_uinfo2, url_pattern="", need_scheme=FALSE)
    test_that("double userinfo is not allowed", {
        expect_equal(length(pu_uinfo1), 0)
        expect_identical(pu_uinfo2, "http://www.example.org/")
    })
    colon_uinfo <- "ftp://:foo@example.org/"
    pu_uinfo3 <- pick_urls(colon_uinfo)
    test_that("\":\" is allowed in userinfo", {
        expect_identical(pu_uinfo3, colon_uinfo)
    })

    double_frag <- "http://www.example.org/#foo#foo"
    double_frag <- c(double_frag, paste0(double_frag, "[]"))
    pu_frag <- pick_urls(double_frag)
    test_that("double fragment is not allowed", {
        expect_identical(pu_frag, rep("http://www.example.org/#foo", 2))
    })

    bad_chars <- "\"<>\\^`{|}%"
    pu_badchar1 <- pick_urls(bad_chars, single_item=FALSE, url_pattern="",
                             need_scheme=FALSE)
    pu_badchar2 <- pick_urls(bad_chars, single_item=TRUE, url_pattern="",
                             need_scheme=FALSE)
    test_that("illegal characters are dropped", {
        expect_equal(length(pu_badchar1), 0)
        expect_equal(length(pu_badchar2), 0)
    })

    ## Email addresses for testing. The results from the final 3 items
    ## do not match the default email_pattern.
    emails <- c("root@127.0.0.1",
                "first.last@example.org",
                "first.last at example.org",
                paste0("(A)foo(comment(nested)\\) (again)) (here)@",
                       "(\\(remove)[vf.a1,b2](foo@[vf.a1,b2])"),
                "nobody@[fd12:3456:789a:1::1]",
                "user@[::1]")
    emails_target1 <- c(emails[1], rep(emails[2], 2))
    emails_target2 <- c(emails_target1, rep("foo@[vf.a1,b2]", 2), emails[5:6])

    pe1_combi <- pick_urls(emails, plain_email=TRUE)
    pe1b_combi <- pick_urls(emails, plain_email=TRUE, email_pattern="")
    pe1c <- pick_urls(emails, plain_email=TRUE, single_item=TRUE)
    test_that("email addresses are found, deobfuscate=TRUE, multiple inputs",{
        expect_equal(length(pe1_combi[[1]]), 0)
        expect_equal(length(pe1b_combi[[1]]), 0)
        expect_identical(pe1_combi[[2]], emails_target1)
        expect_identical(pe1b_combi[[2]], emails_target2)
        expect_identical(pe1c, pe1_combi)
    })
    pe1_combi <- pe1_combi[[2]]
    pe1b_combi <- pe1b_combi[[2]]
    pe1_individual <- lapply(emails, pick_urls, plain_email=TRUE)
    pe1b_individual <- lapply(emails, pick_urls, plain_email=TRUE,
                              email_pattern="")
    pe1_two <- unlist(lapply(pe1_individual, `[[`, 2))
    pe1b_two <- unlist(lapply(pe1b_individual, `[[`, 2))
    test_that("email addresses are found, deobfuscate=TRUE, single inputs",{
        expect_true(all(vapply(lapply(pe1_individual, `[[`, 1),
                               length, numeric(1)) == 0))
        expect_true(all(vapply(lapply(pe1b_individual, `[[`, 1),
                               length, numeric(1)) == 0))
        expect_identical(pe1_combi, pe1_two)
        expect_identical(pe1b_combi, pe1b_two)
    })

    idx_at_char <- grep("@", emails[seq_along(emails_target1)], fixed=TRUE)
    pe2_combi <- pick_urls(emails, plain_email=TRUE, deobfuscate=FALSE)
    test_that("email addresses are found, deobfuscate=FALSE, multiple inputs",{
        expect_equal(length(pe2_combi[[1]]), 0)
        expect_identical(pe2_combi[[2]], emails_target1[idx_at_char])
    })
    pe2_combi <- pe2_combi[[2]]
    pe2_individual <- lapply(emails, pick_urls,
                             plain_email=TRUE, deobfuscate=FALSE)
    pe2_one <- lapply(pe2_individual, `[[`, 1)
    pe2_individual <- unlist(lapply(pe2_individual, `[[`, 2))
    test_that("email addresses are found, deobfuscate=FALSE, single inputs",{
        expect_true(all(vapply(pe2_one, length, numeric(1)) == 0))
        expect_identical(pe2_combi, pe2_individual)
    })

    embedded_email <- c("ftp://anonymous@example.org",
                        "http://www.example.org/?email=user@example.org")
    pe3_combi <- pick_urls(embedded_email, plain_email=TRUE)
    test_that("email addresses inside URLs are not selected, multiple inputs",{
        expect_equal(unname(vapply(pe3_combi, length, numeric(1))), c(2, 0))
        expect_identical(pe3_combi[[1]], embedded_email)
    })
    pe3_individual <- lapply(embedded_email, pick_urls, plain_email=TRUE)
    test_that("email addresses inside URLs are not selected, multiple inputs",{
        expect_true(all(vapply(lapply(pe3_individual, `[[`, 2),
                               length, numeric(1)) == 0))
        expect_identical(pe3_combi[[1]],
                         unlist(lapply(pe3_individual, `[[`, 1)))
    })
    emails_macro <- paste0("\\email{", emails, "}")
    pe_macro <- pick_urls(emails_macro, plain_email=TRUE, email_pattern="")
    test_that("LaTeX macros do not confuse email address picking", {
        expect_equal(length(pe_macro[[1]]), 0)
        expect_identical(pe_macro[[2]], emails_target2)
    })

    good_comment <- "foo(comment)@example.org"
    bad_comment1 <- "foo((comment)@example.org"
    bad_comment2 <- "foo(comment))@example.org"
    bad_comment3 <- "foo\\(comment)@example.org"
    bad_comment4 <- "foo(comment\\)@example.org"
    comm_good <- pick_urls(good_comment, plain_email = TRUE)
    comm_bad1 <- pick_urls(bad_comment1, plain_email = TRUE)
    comm_bad2 <- pick_urls(bad_comment2, plain_email = TRUE)
    comm_bad3 <- pick_urls(bad_comment3, plain_email = TRUE)
    comm_bad4 <- pick_urls(bad_comment4, plain_email = TRUE)
    test_that("balanced parentheses are required in comments", {
        expect_equal(length(comm_good[[1]]), 0)
        expect_identical(comm_good[[2]], "foo@example.org")
        expect_equal(lengths(comm_bad1, use.names = FALSE), c(0, 0))
        expect_identical(comm_bad2, comm_bad1)
        expect_identical(comm_bad3, comm_bad1)
        expect_identical(comm_bad4, comm_bad1)
    })

    nested_email <- "foo1(foo2(foo3@example.org)@example.org)@example.org"
    comm_nested <- pick_urls(nested_email, plain_email = TRUE)
    test_that("internal email comments are skipped", {
        expect_equal(length(comm_nested[[1]]), 0)
        expect_identical(comm_nested[[2]], "foo1@example.org")
    })

    outer_email <- "(foo@example.org(foo@example.org))foo@example.org"
    comm_outer <- pick_urls(outer_email, plain_email = TRUE)
    test_that("external email comments are kept", {
        expect_equal(length(comm_outer[[1]]), 0)
        expect_identical(comm_outer[[2]], rep("foo@example.org", 3))
    })

    pos_bad <- "f(foo2@e(foo3@example.org)xample.org)oo1@example.org"
    comm_pos <- pick_urls(pos_bad, plain_email = TRUE)
    test_that("comments in a bad position break the email address", {
        expect_equal(length(comm_pos[[1]]), 0)
        expect_identical(sort(comm_pos[[2]]),
                         sort(c("foo3@example.org", "oo1@example.org")))
    })

    left_empty <- "(foo@example.org)@example.org"
    right_empty <- "foo@(foo@example.org)"
    empty_lr <- pick_urls(c(left_empty, right_empty), plain_email=TRUE)
    test_that("email address in parentheses is found", {
        expect_equal(length(empty_lr[[1]]), 0)
        expect_identical(empty_lr[[2]], rep("foo@example.org", 2))
    })

    multi_par <- "(((foo@example.org)))(foo@example.org)))))))"
    comm_multi <- pick_urls(multi_par, plain_email = TRUE)
    test_that("multiple parentheses around email address are acceptable", {
        expect_equal(length(comm_multi[[1]]), 0)
        expect_identical(comm_multi[[2]], rep("foo@example.org", 2))
    })

    par_in_quot <- "\"(foo)\"(comment)@example.org"
    quot_par <- pick_urls(par_in_quot, plain_email=TRUE)
    test_that("parentheses survive in a quoted string", {
        expect_equal(length(quot_par[[1]]), 0)
        expect_identical(quot_par[[2]], "\"(foo)\"@example.org")
    })

    embedded_target <- "\"foo(bar@example.org)\"@example.org"
    target2 <- sub("bar", "bar()", embedded_target, fixed=TRUE)
    embedded_address <- c(paste0("http://www.example.org/ ", embedded_target),
                          target2)
    embedded_target <- c(embedded_target, target2)
    addr_emb <- pick_urls(embedded_address, plain_email=TRUE)
    test_that("email address may contain what looks like another address", {
        expect_identical(addr_emb[[1]], "http://www.example.org/")
        expect_identical(addr_emb[[2]], embedded_target)
    })

    bad_iplit <- "user@[abc]"
    ip_bad <- pick_urls(bad_iplit, plain_email=TRUE, email_pattern="")
    test_that("bad IP literal is recognized", {
        expect_equal(lengths(ip_bad, use.names=FALSE), c(0, 0))
    })

    obfu <- c("ab at c.def", "a.b at c.def", "a-b at c.def",
              "a/b at c.def", "a at c.def", "ab at c.de", "10 at c.def",
              "ab at 0.123", "\"foo\" at c.def")
    no_obfu <- sub(" at ", "@", obfu, fixed = TRUE)
    pick_obfu <- pick_urls(obfu, plain_email = TRUE)
    pick_noobfu <- pick_urls(no_obfu, plain_email = TRUE)
    test_that("there are some requirements for obfuscated addresses", {
        expect_equal(length(pick_obfu[[1]]), 0)
        expect_equal(length(pick_noobfu[[1]]), 0)
        expect_identical(pick_obfu[[2]], no_obfu[1:3])
        expect_identical(pick_noobfu[[2]], no_obfu)
    })

    nospace <- c("/*@example.org", "foo@0.1", "a@example.org",
                 "a\n @example.org", "/*\n @example.org", "foo@\n 0.1")
    nospace_sub <- sub("\n ", "", nospace, fixed = TRUE)
    pick_nsp1 <- pick_urls(nospace[1:4], plain_email = TRUE)
    pick_nsp2 <- pick_urls(nospace[5:6], plain_email = TRUE)
    test_that("if there are spaces, letters are required", {
        expect_equal(length(pick_nsp1[[1]]), 0)
        expect_equal(lengths(pick_nsp2, use.names=FALSE), c(0, 0))
        expect_identical(pick_nsp1[[2]], nospace_sub[1:4])
    })

    ## Test mailto URLs
    mailto_emails <- paste0("mailto:",
                            c(emails[1:2], paste0(emails[1:2], collapse=",")))
    pe4_combi <- pick_urls(mailto_emails, plain_email=TRUE)
    pe5_combi <- pick_urls(mailto_emails, all_email=TRUE)
    pe6_combi <- pick_urls(mailto_emails, url_pattern="^mailto:")
    pe7_combi <- pick_urls(mailto_emails, url_pattern="^mailto:",
                           all_email=TRUE)
    test_that("mailto URLs work, multiple inputs", {
        expect_true(all(vapply(pe4_combi, length, numeric(1)) == 0))
        expect_equal(unname(vapply(pe5_combi, length, numeric(1))), c(0, 4))
        expect_identical(pe5_combi[[2]], rep(emails[1:2], 2))
        expect_identical(pe6_combi, mailto_emails)
        expect_identical(pe5_combi, pe7_combi)
    })
    pe4_individual <- lapply(mailto_emails, pick_urls, plain_email=TRUE)
    pe5_individual <- lapply(mailto_emails, pick_urls, all_email=TRUE)
    pe6_individual <- lapply(mailto_emails, pick_urls, url_pattern="^mailto:")
    pe7_individual <- lapply(mailto_emails, pick_urls, url_pattern="^mailto:",
                             all_email=TRUE)
    test_that("mailto URLs work, single inputs", {
        expect_equal(length(unlist(pe4_individual)), 0)
        expect_true(all(vapply(lapply(pe5_individual, `[[`, 1),
                               length, numeric(1)) == 0))
        expect_identical(lapply(pe5_individual, `[[`, 2),
                         list(emails[1], emails[2], emails[1:2]))
        expect_identical(unlist(pe6_individual), pe6_combi)
        expect_identical(pe5_individual, pe7_individual)
    })
    mostly_bad0 <- paste0("mailto:", c("#", "a", "a@b"))
    mostly_bad <- c(mostly_bad0,
                    paste0(",", mostly_bad0),
                    paste0("a,", mostly_bad0),
                    paste0("a,bb,", mostly_bad0))
    pe8 <- pick_urls(mostly_bad, url_pattern="^mailto:")
    pe9 <- pick_urls(mostly_bad, url_pattern="^mailto:", single_item=TRUE)
    test_that("bad email addresses are dropped", {
        expect_identical(pe8, rep("mailto:a@b", 4))
        expect_identical(pe9, pe8)
    })
    mailto_hfields <- paste0(mailto_emails[1], "?foo=bar",
                             c("", "&abc=def", "?abc=def"))
    pe10 <- pick_urls(mailto_hfields, url_pattern="")
    test_that("hfields are accepted in mailto URLs", {
        expect_identical(pe10, mailto_hfields[c(1, 2, 1)])
    })

    bad_cloc1 <- "user(comment)@e(foo)xample.org"
    bad_cloc2 <- "us(foo)er@(comment)example.org"
    good_cloc <- "user(comment)@(comment)example.org"
    pe11a <- pick_urls(bad_cloc1, plain_email=TRUE, email_pattern="")
    pe11b <- pick_urls(bad_cloc2, plain_email=TRUE, email_pattern="")
    pe11c <- pick_urls(good_cloc, plain_email=TRUE, email_pattern="")
    test_that("bad comment locations are recognized", {
        expect_equal(lengths(pe11a, use.names=FALSE), c(0, 0))
        expect_equal(lengths(pe11b, use.names=FALSE), c(0, 0))
        expect_equal(length(pe11c[[1]]), 0)
        expect_identical(pe11c[[2]], "user@example.org")
    })

    ## Combination of URLs and email addresses
    combo_items <- c(urls, emails, mailto_emails)
    combo_phrase <-
        paste0("See ",
               paste0(c(combo_items[-length(combo_items)],
                        paste0("and ", combo_items[length(combo_items)])),
                      collapse = ", "),
               ".")
    pc1_combi <- pick_urls(combo_items, url_pattern="^(mailto|https?|ftp):",
                           plain_email=TRUE)
    pc2_combi <- pick_urls(combo_phrase, url_pattern="^(mailto|https?|ftp):",
                           plain_email=TRUE)
    test_that("mixed URLs and email addresses work, multiple inputs", {
        expect_identical(pc1_combi[[1]], c(norm_urls, mailto_emails))
        expect_identical(pc1_combi[[2]], emails_target1)
        expect_identical(pc1_combi, pc2_combi)
    })
    pc1_individual <-
        lapply(combo_items, pick_urls,
               url_pattern="^(mailto|https?|ftp):", plain_email=TRUE)
    email_idx <- seq(from = length(urls) + 1, by = 1,
                     length.out = length(emails))
    url_idx <- seq_along(combo_items)[-email_idx]
    pc1_email <- pc1_individual[email_idx]
    pc1_url <- pc1_individual[url_idx]
    pc1_email_two <- lapply(pc1_email, `[[`, 2)
    pc1_url_one <- lapply(pc1_url, `[[`, 1)
    test_that("mixed URLs and email addresses work, multiple inputs", {
        expect_true(all(vapply(lapply(pc1_email, `[[`, 1),
                               length, numeric(1)) == 0))
        expect_true(all(vapply(lapply(pc1_url, `[[`, 2),
                               length, numeric(1)) == 0))
        expect_equal(vapply(pc1_email_two, length, numeric(1)),
                     c(1, 1, 1, 0, 0, 0))
        expect_true(all(vapply(pc1_url_one, length, numeric(1)) == 1))
        expect_identical(unlist(pc1_email_two), emails_target1)
        expect_identical(unlist(pc1_url_one), c(norm_urls, mailto_emails))
    })

    embedded_url1 <- "\"http://www.example.org/\"@example.org"
    embedded_url2 <- c(paste0(embedded_url1, ", \"http://www.example.org/\""),
                       paste0("<http://www.example.org/>, ", embedded_url1))
    pc3a <- pick_urls(embedded_url1, plain_email = TRUE)
    pc3b <- pick_urls(embedded_url2, plain_email = TRUE)
    pc3c <- pick_urls(embedded_url2, plain_email = TRUE, collapse_x=TRUE)
    test_that("email address may contain a URL", {
        expect_equal(length(pc3a[[1]]), 0)
        expect_identical(pc3a[[2]], embedded_url1)
        expect_identical(pc3b[[1]], rep("http://www.example.org/", 2))
        expect_identical(pc3b[[2]], rep(embedded_url1, 2))
    })
    test_that("collapse_x does nothing (as expected)", {
        expect_identical(pc3c, pc3b)
    })
    pc4 <- pick_urls(embedded_url1)
    test_that("URL is picked from email if (!(plain_email || all_email))", {
        expect_identical(pc4, "http://www.example.org/")
    })
    pc5 <- pick_urls(paste0(emails[2], "`ftp://", emails[2], "`"),
                     plain_email = TRUE)
    test_that("email address and URL with local part may coexist", {
        expect_identical(pc5[[1]], paste0("ftp://", emails[2]))
        expect_identical(pc5[[2]], emails[2])
    })

    enc_input <- paste(urls[1], urls[2], sep = " \xe4 ")
    Encoding(enc_input) <- "UTF-8"
    pu_enc <- pick_urls(enc_input)
    test_that("valid parts of invalid UTF-8 are used", {
        expect_identical(pu_enc, urls[1:2])
    })

    char_badenc <- "\xe4"
    Encoding(char_badenc) <- "UTF-8"
    bad_target <- "vwxyz://ex"
    bad_url <- paste0(bad_target, char_badenc, "mple.org/", char_badenc)
    Encoding(bad_url) <- "UTF-8"
    bad_a <- paste0(char_badenc, "a", char_badenc)
    Encoding(bad_a) <- "UTF-8"
    bad_target2 <- "http://www.example.org/"
    bad_url2 <- paste0(char_badenc, bad_target2, char_badenc, "a", char_badenc)
    Encoding(bad_url2) <- "UTF-8"
    unk_iri <- bad_url
    Encoding(unk_iri) <- "unknown"
    latin_iri <- bad_url
    Encoding(latin_iri) <- "latin1"

    pu_charbad <- pick_urls(char_badenc)
    test_that("completely unusable input is discarded quietly", {
        expect_equal(length(pu_charbad), 0)
    })

    check_utf8 <- function() {
        pu_bad_a <- pick_urls(bad_url, url_pattern="")
        pu_bad2 <- pick_urls(bad_url2, url_pattern="")
        pu_a <- pick_urls(bad_a, url_pattern="")
        pu_latin <- pick_urls(latin_iri, url_pattern="")
        pu_unk <- pick_urls(unk_iri, url_pattern="")
        test_that("output is good in a UTF-8 locale", {
            expect_identical(pu_bad_a, bad_target)
            expect_identical(pu_bad2, bad_target2)
            expect_equal(length(pu_a), 0)
            expect_identical(pu_latin, latin_iri)
            expect_identical(pu_unk, bad_target)
        })
    }
    check_latin <- function() {
        pu_bad_a <- pick_urls(bad_url, url_pattern="")
        pu_bad2 <- pick_urls(bad_url2, url_pattern="")
        pu_a <- pick_urls(bad_a, url_pattern="")
        pu_latin <- pick_urls(latin_iri, url_pattern="")
        pu_unk <- pick_urls(unk_iri, url_pattern="")
        test_that("output is good in a locale with a single byte encoding", {
            expect_identical(pu_bad_a, bad_target)
            expect_identical(pu_bad2, bad_target2)
            expect_equal(length(pu_a), 0)
            expect_identical(pu_latin, latin_iri)
            expect_identical(pu_unk, latin_iri)
        })
    }
    if (.Platform[["OS.type"]] == "unix") {
        l10 <- l10n_info()
        loc_vec <- c(LC_CTYPE=NA_character_, LC_COLLATE=NA_character_)
        if (!(l10[["UTF-8"]])) {
            utf8_locales <- c("en_US.utf8", "en_US.UTF-8",
                              "de_DE.utf8", "de_DE.UTF-8", "de_DE")
            utf8_found <- FALSE
            for (loc in utf8_locales) {
                loc_vec[] <- loc
                if (suppressWarnings(tryCatch(withr::with_locale(loc_vec,
                                                                 l10n_info()[["UTF-8"]]),
                                              error = function(...) FALSE))) {
                    utf8_found <- TRUE
                    break
                }
            }
            if (utf8_found) {
                withr::with_locale(loc_vec, check_utf8())
            }
        } else {
            check_utf8()
        }
        if (l10[["MBCS"]]) {
            latin_locales <- c("en_US.iso88591", "en_US.ISO8859-1",
                               "de_DE.iso88591", "de_DE.ISO8859-1", "de_DE")
            latin_found <- FALSE
            for (loc in latin_locales) {
                loc_vec[] <- loc
                if (suppressWarnings(tryCatch(withr::with_locale(loc_vec,
                                                                 !(l10n_info()[["MBCS"]])),
                                              error = function(...) FALSE))) {
                    latin_found <- TRUE
                    break
                }
            }
            if (latin_found) {
                withr::with_locale(loc_vec, check_latin())
            }
        } else {
            check_latin()
        }
    } else if (!(l10n_info()[["MBCS"]])) {
        check_latin()
    }

    ## Test collapse_x = TRUE
    coll_input1 <-
        c("Testing if split URL <http://en.wikipedia.org/wiki/",
          "R_(programming_language)> can be extracted",
          "foo>")
    coll_input1[1] <- paste(urls[1], coll_input1[1], sep = ", ")
    coll_input1[2] <- paste(coll_input1[2], urls[2], sep = ", ")
    coll1a <- pick_urls(coll_input1, collapse_x=TRUE)
    coll1b <- pick_urls(coll_input1, collapse_x=TRUE, plain_email=TRUE)
    test_that("collapse_x = TRUE works for URLs", {
        expect_identical(sort(coll1a), sort(urls[c(1, 2, 4)]))
        expect_identical(coll1a, coll1b[[1]])
    })
    coll_input2 <- c(coll_input1[1], NA_character_, coll_input1[2:3])
    coll2 <- pick_urls(coll_input2, collapse_x=TRUE)
    test_that("NA string interrupts collapsing (URLs)", {
        expect_identical(coll2, c(urls[1], "http://en.wikipedia.org/wiki/",
                                  urls[2]))
    })
    coll_input3 <-
        c("Testing split emails \"(test\"())", " \"local",
          " part\"@example.org, foo@",
          " example.org, bar", " @example.org, obfu at",
          " example.org, scated", " at example.org, foo@[vf",
          " .a1,b2], user1 (comment)", " (comment)@example.org, user2",
          " \t (comment)@example.org, user3(comment)",
          " @example.org, user4(comm", " ent)@example.org(", "last")
    coll3 <- pick_urls(coll_input3, collapse_x=TRUE, plain_email=TRUE,
                       email_pattern="")
    coll3b <- pick_urls(as.vector(rbind(coll_input3, NA_character_)),
                        collapse_x=TRUE, plain_email=TRUE, email_pattern="")
    coll3c <- pick_urls(coll_input3, plain_email=TRUE, email_pattern="")
    coll3_target <-
        c("\"local part\"@example.org", "foo@example.org",
          "bar@example.org", "obfu@example.org", "scated@example.org",
          "foo@[vf.a1,b2]", "user1@example.org", "user2@example.org",
          "user3@example.org", "user4@example.org")
    test_that("collapse_x = TRUE works for email addresses", {
        expect_equal(length(coll3[[1]]), 0)
        expect_identical(coll3[[2]], coll3_target)
        expect_equal(lengths(coll3b, use.names=FALSE), c(0, 0))
        expect_equal(lengths(coll3c, use.names=FALSE), c(0, 0))
    })
    coll_input4a <- c("test1@", NA_character_, " example.org",
                      "test2@", " example.org")
    coll_input4b <- c("test1@", " example.org", NA_character_,
                      "test2@", " example.org")
    coll_input4c <- c("test1@", NA_character_, " example.org",
                      "test2@", NA_character_, " example.org", "test3@")
    coll_input4d <- c("test1@", NA_character_, " example.org", "test2",
                      NA_character_, " @example.org", "test3@")
    coll_input4e <- c("foo", NA_character_, " bar", " test1(comm",
                      NA_character_, " ent)@example.org", "test2(",
                      NA_character_, " foo)@example.org", "test3@")
    coll_input4f <- c("test1(comm", " e", " nt)@example.org",
                      "test2(", " foo)@example.org", "test3@")
    coll4a <- pick_urls(coll_input4a, collapse_x=TRUE, plain_email=TRUE,
                        email_pattern="")
    coll4b <- pick_urls(coll_input4b, collapse_x=TRUE, plain_email=TRUE,
                        email_pattern="")
    coll4c <- pick_urls(coll_input4c, collapse_x=TRUE, plain_email=TRUE,
                        email_pattern="")
    coll4d <- pick_urls(coll_input4d, collapse_x=TRUE, plain_email=TRUE,
                        email_pattern="")
    coll4e <- pick_urls(coll_input4e, collapse_x=TRUE, plain_email=TRUE,
                        email_pattern="")
    coll4f <- pick_urls(coll_input4f, collapse_x=TRUE, plain_email=TRUE,
                        email_pattern="")
    test_that("NA strings interrupts collapsing (emails)", {
        expect_equal(length(coll4a[[1]]), 0)
        expect_identical(coll4a[[2]], "test2@example.org")
        expect_equal(length(coll4b[[1]]), 0)
        expect_identical(coll4b[[2]],
                         c("test1@example.org", "test2@example.org"))
        expect_equal(lengths(coll4c, use.names=FALSE), c(0, 0))
        expect_equal(lengths(coll4d, use.names=FALSE), c(0, 0))
        expect_equal(lengths(coll4e, use.names=FALSE), c(0, 0))
        expect_identical(coll4f, coll4b)
    })
    coll_input5a <- c("\"f", " o", " o\"@example.org")
    coll_input5b <- c("\"f", "o", " o\"@example.org")
    coll_input5c <- c("\"f", " o", "o\"@example.org")
    coll5a <- pick_urls(coll_input5a, collapse_x=TRUE, plain_email=TRUE,
                        email_pattern="")
    coll5b <- pick_urls(coll_input5b, collapse_x=TRUE, plain_email=TRUE,
                        email_pattern="")
    coll5c <- pick_urls(coll_input5c, collapse_x=TRUE, plain_email=TRUE,
                        email_pattern="")
    test_that("lack of initial space interrupts collapsing (email)", {
        expect_equal(length(coll5a[[1]]), 0)
        expect_identical(coll5a[[2]], "\"f o o\"@example.org")
        expect_equal(lengths(coll5b, use.names=FALSE), c(0, 0))
        expect_equal(lengths(coll5c, use.names=FALSE), c(0, 0))
    })
    coll_input6 <- c("\"fo", " o1\"@example.org, \"fo", " o2\"@example.org")
    coll6 <- pick_urls(coll_input6, collapse_x=TRUE, plain_email=TRUE,
                       email_pattern="")
    test_that("collapsing can continue where it stopped (quotes in emails)", {
        expect_equal(length(coll6[[1]]), 0)
        expect_identical(coll6[[2]],
                         c("\"fo o1\"@example.org", "\"fo o2\"@example.org"))
    })
    coll_input7 <- c("<http://www.example", ".org/><http://www.",
                     "example.org/>")
    coll7 <- pick_urls(coll_input7, collapse_x=TRUE)
    test_that("collapsing can continue where it stopped (URLs)", {
        expect_identical(coll7, rep("http://www.example.org/", 2))
    })
    coll_input8 <- c("foo1(comm", " ent1)@example.org, foo2(c",
                     " omment2)@example.org")
    coll8 <- pick_urls(coll_input8, collapse_x=TRUE, plain_email=TRUE,
                       email_pattern="")
    test_that("collapsing can continue where it stopped (comments in emails)", {
        expect_equal(length(coll8[[1]]), 0)
        expect_identical(coll8[[2]],
                         c("foo1@example.org", "foo2@example.org"))
    })
    coll_input9a <- c("foo1", " @example.org, foo2",
                      " @example.org")
    coll_input9b <- c("foo1@", " example.org, foo2",
                      " @example.org")
    coll_input9c <- c("foo1@", " example.org, foo2@",
                      " example.org")
    coll9a <- pick_urls(coll_input9a, collapse_x=TRUE, plain_email=TRUE,
                        email_pattern="")
    coll9b <- pick_urls(coll_input9b, collapse_x=TRUE, plain_email=TRUE,
                        email_pattern="")
    coll9c <- pick_urls(coll_input9c, collapse_x=TRUE, plain_email=TRUE,
                        email_pattern="")
    test_that("collapsing can continue where it stopped (@)", {
        expect_equal(length(coll9a[[1]]), 0)
        expect_identical(coll9a[[2]],
                         c("foo1@example.org", "foo2@example.org"))
        expect_identical(coll9b, coll9a)
        expect_identical(coll9c, coll9a)
    })
    coll_input10 <- c("foo1@[vf", " .a1,b2], foo2@[vf.a1", " ,b2]")
    coll10 <- pick_urls(coll_input10, collapse_x=TRUE, plain_email=TRUE,
                        email_pattern="")
    test_that("collapsing can continue where it stopped (IP literals)", {
        expect_equal(length(coll10[[1]]), 0)
        expect_identical(coll10[[2]],
                         c("foo1@[vf.a1,b2]", "foo2@[vf.a1,b2]"))
    })
    coll_input11 <- c("\"f", " o", " o@example.org")
    coll11 <- pick_urls(coll_input11, collapse_x=TRUE, plain_email=TRUE,
                        email_pattern="")
    test_that("no collapsing (but cannot really be tested)", {
        expect_equal(length(coll11[[1]]), 0)
        expect_identical(coll11[[2]], "o@example.org")
    })

    coll_input12 <- c("a", " b", NA_character_,
                      "foo", " (comment)@example.org")
    coll12 <- pick_urls(coll_input12, collapse_x=TRUE, plain_email=TRUE,
                        email_pattern="")
    test_that("collapse_x stress test (12) passes", {
        expect_equal(length(coll12[[1]]), 0)
        expect_identical(coll12[[2]], "foo@example.org")
    })
    coll_input13 <- c("a", " (comm)a", "foo", " (comment)@example.org")
    coll13 <- pick_urls(coll_input13, collapse_x=TRUE, plain_email=TRUE,
                        email_pattern="")
    test_that("collapse_x stress test (13) passes", {
        expect_identical(coll13, coll12)
    })
    coll_input14 <- c("a", " foo()@example.org")
    coll14 <- pick_urls(coll_input14, collapse_x=TRUE, plain_email=TRUE,
                        email_pattern="")
    test_that("collapse_x stress test (14) passes", {
        expect_identical(coll14, coll12)
    })
    coll_input15 <- c("a", " foo@()example.org()", "")
    coll15 <- pick_urls(coll_input15, collapse_x=TRUE, plain_email=TRUE,
                        email_pattern="")
    test_that("collapse_x stress test (15) passes", {
        expect_identical(coll15, coll12)
    })
    coll_input16a <- c("foo@(comm)", " (ent)example.org")
    coll_input16b <- c("(foo@(comm))", " ((ent)example.org)")
    coll16a <- pick_urls(coll_input16a, collapse_x=TRUE, plain_email=TRUE,
                         email_pattern="")
    coll16b <- pick_urls(coll_input16b, collapse_x=TRUE, plain_email=TRUE,
                         email_pattern="")
    test_that("collapse_x stress test (16) passes", {
        expect_identical(coll16a, coll12)
        expect_equal(lengths(coll16b, use.names=FALSE), c(0, 0))
    })

    coll_input17a <- c("", NA_character_, "user@[", " ::1]")
    coll_input17b <- sub("@", "", coll_input17a, fixed=TRUE)
    coll_input17c <- sub(" ", "", coll_input17a, fixed=TRUE)
    coll17a <- pick_urls(coll_input17a, collapse_x=TRUE, plain_email=TRUE,
                         email_pattern="")
    coll17b <- pick_urls(coll_input17b, collapse_x=TRUE, plain_email=TRUE,
                         email_pattern="")
    coll17c <- pick_urls(coll_input17c, collapse_x=TRUE, plain_email=TRUE,
                         email_pattern="")
    test_that("collapse_x stress test (17) passes", {
        expect_equal(length(coll17a[[1]]), 0)
        expect_identical(coll17a[[2]], "user@[::1]")
        expect_equal(lengths(coll17b, use.names=FALSE), c(0, 0))
        expect_identical(coll17c, coll17b)
    })

    coll_input18 <- c("a", " [", "g@", NA_character_, "user@[", " ::1]")
    coll18 <- pick_urls(coll_input18, collapse_x=TRUE, plain_email=TRUE,
                        email_pattern="")
    test_that("collapse_x stress test (18) passes", {
        expect_identical(coll18, coll17a)
    })
    coll_input19 <- c("user@[", " ", NA_character_, " ::1]")
    coll19 <- pick_urls(coll_input19, collapse_x=TRUE, plain_email=TRUE,
                        email_pattern="")
    test_that("collapse_x stress test (19) passes", {
        expect_identical(coll19, coll17b)
    })
    coll_input20 <- c("user@[", " ", " ::1]")
    coll20 <- pick_urls(coll_input20, collapse_x=TRUE, plain_email=TRUE,
                        email_pattern="")
    test_that("collapse_x stress test (20) passes", {
        expect_identical(coll20, coll17b)
    })
    coll_input21 <- c("user@[", " user@[", " ::1]")
    coll21 <- pick_urls(coll_input21, collapse_x=TRUE, plain_email=TRUE,
                        email_pattern="")
    test_that("collapse_x stress test (21) passes", {
        expect_identical(coll21, coll17a)
    })
    coll_input22 <- c("foo", " (bar)", " @example.org")
    coll22 <- pick_urls(coll_input22, collapse_x=TRUE, plain_email=TRUE)
    test_that("collapse_x stress test (22) passes", {
        expect_equal(length(coll22[[1]]), 0)
        expect_identical(coll22[[2]], "foo@example.org")
    })
    coll_input23 <- c("foo", " \"(bar)\"", " @example.org")
    coll23 <- pick_urls(coll_input23, collapse_x=TRUE, plain_email=TRUE)
    test_that("collapse_x stress test (23) passes", {
        expect_equal(length(coll23[[1]]), 0)
        expect_identical(coll23[[2]], "\"(bar)\"@example.org")
    })
    coll_input24a <- c("user@", " (foo", " (bar))example.org")
    coll_input24b <- c("user@", " (foo", "(bar))example.org")
    coll_input24c <- c("user@", " (foo", "bar\\(\\)example.org")
    coll24a <- pick_urls(coll_input24a, collapse_x=TRUE, plain_email=TRUE)
    coll24b <- pick_urls(coll_input24b, collapse_x=TRUE, plain_email=TRUE)
    coll24c <- pick_urls(coll_input24c, collapse_x=TRUE, plain_email=TRUE)
    test_that("collapse_x stress test (24) passes", {
        expect_equal(length(coll24a[[1]]), 0)
        expect_identical(coll24a[[2]], "user@example.org")
        expect_equal(lengths(coll24b, use.names=FALSE), c(0, 0))
        expect_equal(lengths(coll24c, use.names=FALSE), c(0, 0))
    })
    coll_input25 <- c("\"foo", " bar\"@example.org \"abc\"", " a@")
    coll25 <- pick_urls(coll_input25, collapse_x=TRUE, plain_email=TRUE)
    test_that("collapse_x stress test (25) passes", {
        expect_equal(length(coll25[[1]]), 0)
        expect_identical(coll25[[2]], "\"foo bar\"@example.org")
    })

    stress1 <- pick_urls("mailto:root@[vf.a1,b2],http://[vf.a1,b2]",
                         url_pattern="", all_email=TRUE, email_pattern="")
    test_that("stress test 1 passes", {
        expect_identical(stress1[[1]], "http://[vf.a1,b2]")
        expect_identical(stress1[[2]], "root@[vf.a1,b2]")
    })
    stress_in2 <- "<foo,url:www.example.org>"
    stress2a <- pick_urls(stress_in2, single_item=TRUE,
                          url_pattern="", need_scheme=FALSE)
    stress2b <- pick_urls(stress_in2, single_item=FALSE,
                          url_pattern="", need_scheme=FALSE)
    stress2c <- pick_urls(stress_in2, single_item=FALSE,
                          url_pattern="", need_scheme=TRUE)
    test_that("stress test 2 passes", {
        expect_equal(length(stress2a), 0)
        expect_less_than(length(stress2b), 3)
        expect_false(any(grepl("url", stress2b, fixed=TRUE)))
        expect_equal(length(stress2c), 0)
    })
    stress_in3 <- c("url:www.example.org/", "http://www.example.org/")
    stress3 <- pick_urls(stress_in3, single_item=TRUE)
    test_that("stress test 3 passes", {
        expect_identical(stress3, stress_in3[2])
    })
    stress_in4 <- c("[vf.sdfs:asdf]/1", ".http://www.example.org/")
    stress4 <- pick_urls(stress_in4, single_item=TRUE)
    test_that("stress test 4 passes", {
        expect_identical(stress4, sub("^[[:punct:]]+", "", stress_in4[2]))
    })
    stress_in5 <- "http://www.example.org/,user@example.org##"
    stress5 <- pick_urls(stress_in5, plain_email=TRUE)
    test_that("stress test 5 passes", {
        expect_identical(stress5[[1]], "http://www.example.org/")
        expect_identical(stress5[[2]], "user@example.org")
    })
    stress_in6 <- "mailto:user1@example.org,user2@example.org?a=b"
    stress6a <- pick_urls(stress_in6, plain_email=TRUE, url_pattern="")
    stress6b <- pick_urls(stress_in6, all_email=TRUE)
    test_that("stress test 6 passes", {
        expect_identical(stress6a[[1]], stress_in6)
        expect_equal(length(stress6a[[2]]), 0)
        expect_equal(length(stress6b[[1]]), 0)
        expect_identical(stress6b[[2]],
                         paste0("user", c("1", "2"), "@example.org"))
    })
    stress_in7 <- "a,,mailto:"
    stress7 <- pick_urls(stress_in7, single_item=TRUE, url_pattern="")
    test_that("stress test 7 passes", {
        expect_equal(length(stress7), 0)
    })
    stress_in8 <- "(http://www.example.org/),foo"
    stress8 <- pick_urls(stress_in8)
    test_that("stress test 8 passes", {
        expect_identical(stress8, "http://www.example.org/")
    })
    stress_in9 <- "<mailto:>"
    stress9 <- pick_urls(stress_in9, url_pattern="", need_scheme=FALSE)
    test_that("stress test 9 passes", {
        expect_equal(length(stress9), 0)
    })
    stress_in10 <- "foo(user@example.org)@[abc]"
    stress10a <- pick_urls(stress_in10, plain_email=TRUE, single_item=TRUE,
                           email_pattern="")
    stress10b <- pick_urls(stress_in10, plain_email=TRUE, single_item=FALSE,
                           email_pattern="")
    test_that("stress test 10 passes", {
        expect_equal(length(stress10a[[1]]), 0)
        expect_identical(stress10a[[2]], "user@example.org")
        expect_identical(stress10b, stress10a)
    })
    stress_in11a <- "mailto:user@example.org"
    stress_in11b <- "mailto:%user@example.org"
    stress11a <- pick_urls(stress_in11a, plain_email=TRUE, url_pattern="")
    stress11b <- pick_urls(stress_in11b, plain_email=TRUE, url_pattern="")
    test_that("mailto URLs have more requirements than plain email addresses",{
        expect_identical(stress11a[[1]], stress_in11a)
        expect_equal(length(stress11a[[2]]), 0)
        expect_equal(length(stress11b[[1]]), 0)
        expect_identical(stress11b[[2]], "%user@example.org")
    })
    stress_in12 <- c(",URL:www.example.org/, ",
                     ",http://www.example.org/,")
    stress12 <- pick_urls(stress_in12, single_item = TRUE)
    test_that("stress test 12 passes", {
        expect_identical(stress12, "http://www.example.org/")
    })

    foomail <- "e-mail:foo@bar.org"
    email1 <- pick_urls(foomail, url_pattern="")
    email2 <- pick_urls(foomail, plain_email=TRUE,
                        mailto_alias=NULL, scheme_sub=NULL)
    email3 <- pick_urls(foomail, all_email=TRUE)
    test_that("mailto_alias works", {
        expect_identical(email1, "mailto:foo@bar.org")
        expect_equal(lengths(email2, use.names=FALSE), c(0, 0))
        expect_equal(length(email3[[1]]), 0)
        expect_identical(email3[[2]], "foo@bar.org")
    })

    unb_input_a1 <- paste0("(", urls[1], " ", urls[4], "))")
    unb_input_a2 <- paste0(unb_input_a1, ")")
    unb_input_b1 <- paste0("(", urls[1], " ", urls[5], "))")
    unb_input_b2 <- paste0(unb_input_b1, ")")
    unb_input_c1 <- paste0("(", urls[1], ") ", urls[4], ")")
    unb_input_c2 <- paste0(unb_input_c1, ")")
    unb_input_d1 <- paste0("(", urls[1], ") ", urls[5], ")")
    unb_input_d2 <- paste0(unb_input_d1, ")")
    unbalanced_a1 <- pick_urls(unb_input_a1)
    unbalanced_a2 <- pick_urls(unb_input_a2)
    unbalanced_b1 <- pick_urls(unb_input_b1)
    unbalanced_b2 <- pick_urls(unb_input_b2)
    unbalanced_c1 <- pick_urls(unb_input_c1)
    unbalanced_c2 <- pick_urls(unb_input_c2)
    unbalanced_d1 <- pick_urls(unb_input_d1)
    unbalanced_d2 <- pick_urls(unb_input_d2)
    test_that("parentheses are counted correctly", {
        expect_identical(unbalanced_a1, c(urls[1], paste0(urls[4], ")")))
        expect_identical(unbalanced_a2, c(urls[1], paste0(urls[4], "))")))
        expect_identical(unbalanced_b1, c(urls[1], paste0(urls[5], ")")))
        expect_identical(unbalanced_b2, c(urls[1], paste0(urls[5], "))")))
        expect_identical(unbalanced_c1, unbalanced_a1)
        expect_identical(unbalanced_c2, unbalanced_a2)
        expect_identical(unbalanced_d1, unbalanced_b1)
        expect_identical(unbalanced_d2, unbalanced_b2)
    })

    a63_input <- paste0("\"a http://www.example.org/\"@",
                        paste0(rep("a", 63), collapse=""))
    a64_input <- paste0(a63_input, "a")
    a63 <- pick_urls(a63_input, plain_email=TRUE, email_pattern="")
    a64 <- pick_urls(a64_input, plain_email=TRUE, email_pattern="")
    test_that("there is a limit to domain name label length", {
        expect_equal(length(a63[[1]]), 0)
        expect_equal(length(a64[[2]]), 0)
        expect_identical(a63[[2]], a63_input)
        expect_identical(a64[[1]], "http://www.example.org/")
    })

    char_bytes <- char_badenc
    Encoding(char_bytes) <- "bytes"
    test_that("bad arguments are caught", {
        expect_error(pick_urls("", plain_email = 0), "is\\.logical")
        expect_error(pick_urls("", plain_email = NA), "!is\\.na")
        expect_error(pick_urls("", plain_email = c(TRUE, TRUE)), "length")
        expect_error(pick_urls("", plain_email = list(TRUE)), "is\\.logical")
        expect_error(pick_urls("", single_item = 0), "is\\.logical")
        expect_error(pick_urls("", single_item = NA), "!is\\.na")
        expect_error(pick_urls("", single_item = c(TRUE, TRUE)), "length")
        expect_error(pick_urls("", single_item= list(TRUE)), "is\\.logical")
        expect_error(pick_urls("", all_email = 0), "is\\.logical")
        expect_error(pick_urls("", all_email = NA), "!is\\.na")
        expect_error(pick_urls("", all_email = c(TRUE, TRUE)), "length")
        expect_error(pick_urls("", all_email = list(TRUE)), "is\\.logical")
        expect_error(pick_urls("", collapse_x = 0), "is\\.logical")
        expect_error(pick_urls("", collapse_x = NA), "!is\\.na")
        expect_error(pick_urls("", collapse_x = c(TRUE, TRUE)), "length")
        expect_error(pick_urls("", collapse_x = list(TRUE)), "is\\.logical")
        expect_error(pick_urls("", deobfuscate = 0), "is\\.logical")
        expect_error(pick_urls("", deobfuscate = NA), "!is\\.na")
        expect_error(pick_urls("", deobfuscate = c(TRUE, TRUE)), "length")
        expect_error(pick_urls("", deobfuscate = list(TRUE)), "is\\.logical")
        expect_error(pick_urls("", need_scheme = 0), "is\\.logical")
        expect_error(pick_urls("", need_scheme = NA), "!is\\.na")
        expect_error(pick_urls("", need_scheme = c(TRUE, TRUE)), "length")
        expect_error(pick_urls("", need_scheme = list(TRUE)), "is\\.logical")
        expect_error(pick_urls("", rm_endpunct = 2.5))
        expect_error(pick_urls("", rm_endpunct = NA_real_))
        expect_error(pick_urls("", rm_endpunct = NA))
        expect_error(pick_urls("", rm_endpunct = c(TRUE, TRUE)))
        expect_error(pick_urls("", rm_endpunct = list(TRUE)))
        expect_error(pick_urls("", mailto_alias = 0))
        expect_error(pick_urls("", mailto_alias = TRUE))
        expect_error(pick_urls("", mailto_alias = list(TRUE)))
        expect_error(pick_urls("", mailto_alias = list("foo")))
        expect_error(pick_urls("", scheme_sub = 0))
        expect_error(pick_urls("", scheme_sub = TRUE))
        expect_error(pick_urls("", scheme_sub = "foo"))
        expect_error(pick_urls("", scheme_sub = list("abc", b="foo")))
        expect_error(pick_urls("", scheme_sub = list(a=1, b="foo")))
        expect_error(pick_urls("", scheme_sub = list(a="foo",
                                                     b=c("x", char_badenc))))
        expect_error(pick_urls("", url_pattern = 0))
        expect_error(pick_urls("", url_pattern = TRUE))
        expect_error(pick_urls("", url_pattern = list("foo")))
        expect_error(pick_urls("", url_pattern = c("foo", "bar")))
        expect_error(pick_urls("", url_pattern = NA_character_))
        expect_error(pick_urls("", url_pattern = char_badenc))
        expect_error(pick_urls("", url_pattern = char_bytes))
        expect_error(pick_urls("", plain_email=TRUE, email_pattern=0))
        expect_error(pick_urls("", plain_email=TRUE, email_pattern=TRUE))
        expect_error(pick_urls("", plain_email=TRUE,
                               email_pattern = c("foo", "bar")))
        expect_error(pick_urls("", plain_email=TRUE,
                               email_pattern=list("foo")))
        expect_error(pick_urls("", plain_email=TRUE,
                               email_pattern = NA_character_))
        expect_error(pick_urls("", plain_email=TRUE,
                               email_pattern = char_badenc))
        expect_error(pick_urls("", plain_email=TRUE,
                               email_pattern = char_bytes))
    })

    test_that("no (valid) input strings => no output", {
        expect_equal(length(pick_urls("")), 0)
        expect_equal(length(pick_urls("", single_item=TRUE)), 0)
        expect_equal(length(pick_urls(char_bytes)), 0)
        expect_equal(lengths(pick_urls("", plain_email=TRUE),
                             use.names=FALSE), c(0, 0))
        expect_equal(lengths(pick_urls(char_bytes, plain_email=TRUE),
                             use.names=FALSE), c(0, 0))
    })
}
test_results()

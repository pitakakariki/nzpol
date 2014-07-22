#
# Examples from [wiseman]
#

# http://www.jdawiseman.com/papers/electsys/apportionment.html

context("Apportionment")

test_that("Wiseman examples work", {
    
    # First example
    
    expect_identical(
        Hamilton(c(59,26,16,7), 11),
        c(6, 3, 1, 1)
    )

    # Alabama paradox
    
    expect_identical(
        Hamilton(c(5,3,1), 4),
        c(2, 1, 1)
    )

    expect_identical(
        Hamilton(c(5,3,1), 5),
        c(3, 2, 0)
    )
    
    # Minor effect
    
    expect_identical(
        Hamilton(c(50,17,4,0), 11),
        c(8, 3, 0, 0)
    )

    expect_identical(
        Hamilton(c(50,17,4,1), 11),
        c(8, 2, 1, 0)
    )
    
    expect_identical(
        Hamilton(c(50,17,4,2), 11),
        c(7, 3, 1, 0)
    )
    
    # Webster and Jefferson
    # i.e. Sainte-Laguë and d'Hondt

    expect_equivalent(
        SainteLague(c(59,26,16,7), 11),    
        c(6, 2, 2, 1)
    )

    expect_equivalent(
        SainteLague(c(4,1), 3),
        c(2, 1)
    )

    expect_equivalent(
        dHondt(c(59,26,16,7), 11),    
        c(7, 3, 1, 0)
    )
        
    expect_equivalent(
        dHondt(c(4,1), 3),
        c(3, 0)
    )
    
    # Danish, Imperiali, modified Sainte-Laguë

    expect_equivalent(
        Danish(c(59,26,16,7), 11),    
        c(5, 3, 2, 1)
    )

    expect_equivalent(
        Imperiali(c(59,26,16,7), 11),    
        c(8, 2, 1, 0)
    )

    expect_equivalent(
        mSainteLague(c(59,26,16,7), 11),    
        c(6, 3, 2, 0)
    )

})

test_that("Additional examples work", {

    # extra from NZ election 2005
    
    expect_identical(
        quotient(c(935319,889813,130115,34469,120521,60860,48263,26441), 120),
        structure(c(50, 48, 7, 2, 6, 3, 3, 1), iterations = 0)

    )
    
    # zero votes has to work
    
    expect_equivalent(
        quotient(c(0,100), 100),
        c(0, 100)
    )
    
    expect_identical(
        quotient(c(0, 100), 10, c(10, 0)),
        structure(c(0, 10), iterations = 10)
    )

    # repeats using quotient
    
    expect_identical(
        quotient(c(53,24,23), 7),
        structure(c(3, 2, 2), iterations = 1)
    )

    expect_identical(
        quotient(c(59,26,16,7), 11),
        structure(c(6, 2, 2, 1), iterations = 1)
    )
    
})


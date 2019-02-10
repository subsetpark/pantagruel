# Expense Calculation: A Case Study

Let's look at an example of how Pantagruel would be used to specify a
simple real-world program.

The Double-A minor league baseball team, the Peoria Sweathogs,
wants their dev team to put together a small program to help them
with accounting. They keep track of petty cash with a simple CSV file,
where each line is an expense. Each line records the person who incurred
the expense, what it was for, and how much. The accounting department,
such as it is, reimburses all parties at the end of every month.

What has made things more complicated in the recent past is that groups of
people within the organization have started submitting expenses as single
row. The pitching staff, for instance, might submit a single expense, and
it's up to accounting to reimburse each member individually. Accounting
agrees to this, with the stipulation that they'll only divide the charges
equally when they have to be distributed.

To accomplish this they have asked their dev team to put together a
software package. They need a program they can run on a CSV file that
will expand every group entry into a series of entries for each member
of the group.

## A ticket

Maybe the most common way that such a thing would get done is that
the head of accounting would wander over to the desk of the CTO of the
Peoria Sweathogs and explain the situation, and describe what they'd
like (the dev team has been using a VC-backed tip-splitting SAAS startup
internally for months, and thus is unaware of the accounting practices
of the rest of the Sweathogs organization).

This is suboptimal; much can be misunderstood in a conversation, and for
reference the lead developer might only have a post-it note the CTO jotted
down during the chat, which might have left out some important details. So
the CTO asks accounting to work with an internal product manager to put
together a ticket.

This is what they put together.

> Accounting would like to be able to handle expense reports for groups of people.
>
> Accounting currently uses a CSV file to keep track of all petty-cash
> reimbursement requests. They need the ability to handle expense reports
> that are from multiple people.
>
> They should be able to process a monthly expense report so that any
> group expenses are turned into individual expenses, which they can then
> process normally.

This is not bad. It describes the problem being encountered, specifies a
couple of acceptance criteria, and is free of jargon. However, natural
language is not perfectly suited to specifying software, and so there
are some gaps.

The first is that some of the language here is a bit confusing if you
don't already know the system that accounting uses. Are reimbursement
requests the same as expense reports? How does a monthly expense report
relate to a normal expense report? Or are they even two different things?

The second is that, because this ticket is admirably reticent about the
*implementation* of the feature, it ends up leaving quite a bit unsaid
about the parameters and constraints of the problem. What do the group
expense reports look like? Do they have the same format as the individual
ones? What if they contain a group with zero members? What if they refer
to a group that we don't know about?

The third is that as a *ticket* this document doesn't actually specify
very much at all; it describes the desired *change* in the behavior of
an existing software system, but even if it's quite detailed about the
change, it doesn't say anything about the existing format of the CSV
or what it means to process a reimbursement request. Those things are
presumably unchanged, but this document relies on existing developer
familiarity to make the ticket actionable.

### The limits of natural language specification

None of these objections is insurmountable. But they do end up colliding
with each other somewhat uneasily.

For instance, we could express our new software requirement as an
exhaustive document of cases and acceptance criteria. The format of
an expense will look like *this*. When *this* edge case is encountered,
*this* behavior should occur. That's quite a bit more reading and writing,
but certainly possible and would probably leave developers much more
equipped.

On the other hand, if we wanted a true specification rather than a ticket,
we'd be making our job much more difficult if we needed to update a
long document listing all the potential behaviors of the program. If we
wanted to be able to document the whole process and formats involved, we
certainly could; but then how would we communicate some new change that
had to happen? Would we need to write the acceptance criteria and then
go back and edit our specification as well so they agreed? Even if we
just had a specification and went through and edited it, would we expect
a developer to read through the whole thing and notice what was different?

In other words, we would like more specificity and rigor in our program
specifications, and we'd like living, complete reference specifications
rather than one-off tickets. But it seems like every step we take towards
those goals results in documents that increasingly difficult to read
and keep up to date.

## Writing a specification

Actually, the program we've described is quite simple. You could probably
keep the whole thing in your head if you were the person who came up
with it.[^1] And the code would be fairly straightforward to write. That
makes us suspect that it should be fairly simple to describe, too. What
we need is a language for describing our expectations that is designed
to express these sorts of thoughts. That language exists, and it's called
mathematics. We should write a Pantagruel specification that more tersely
and less ambiguously communicates our expectations.

[^1]: That's one of the pitfalls, too; it's simple, so it's easy to
understand, but there are a multitude of extremely similar behaviors
which are all equally simple and maybe just as easy to arrive at from
having the program described to you. So simplicity is not a reason to
leave the behavior implicit.

Here's a document that attempts to specify the behavior of our program:

```pantagruel
remove_groups report: Report.
" An expense report consists of an ordered series of reimbursement requests,
" each of which has a requester, a memo, and and amount.
Report <= [Request].
req (requester, memo):String, amount: Nat => Request.
---
all r: report ..
    (r.requester in group_table ->
        ~(r in report')
        and
        all group_member:(group_table r.requester) ..
            (req group_member r.memo (r.amount % #(group_table r.requester))) in report'
    )
    and (~(r.requester in group_table) -> r in report').

;
" Groups are tracked in a table which relates a group name to the names
" of its members.
group_table requester_name: String :: {String}.
```

This attempts to describe the exact effects of a procedure called
`remove_groups`. After we run it on some `report`, we expect that
any reimbursement request whose requester is found in a table called
`group_table` should be removed, and in its place we should find one new
request for each member of that group, with the amount divided equally
among them.

There's much to add, of course; some of the edge cases mentioned above
aren't defined. It doesn't say, for instance, what should happen if the
entry in the group table has no members (we might add an invariant that
states that a group always has at least one member). However, the fact
that this has taken the form of a total specification, which we intend
to keep up to date and authoritative, means that such details can be
determined and added to the spec. If this were just a ticket describing
changes needed, such details are more likely to languish in the discussion
around the ticket---in comments, or even worse, subsequent conversations
between the dev and product owner.

However, other questions have been nailed down as a result of our
being forced to use concrete notation to express our expectations. We
have given things names, for example: we have designated the CSV file
as a *Report* and each row in it as a *Request*. We've also specified
what happens when "it refers to a group we don't know about": nothing,
because by specification our program leaves any row untouched if its
requester is not in our group table.

We have also been forced to flesh out the workings of the program:
there is some mapping between group names and members called the
`group_table`. But we only have to describe it in terms of relations. We
did not have to say anything about whether it was another CSV, or a SQL
database, or a key-value store, or a server. We had to reason through
its workings enough to be able to express our intuitive understanding of
the program in an explicit way.

Using this spec we can try to write a Python script that will process
an expenses CSV.

```python
import csv

GROUP_MAP = {
    "Pitching Staff": {"Nolan Ryan", "Orel Hershiser"},
    "Communications": {"Jeremy Goodwin"}
}

with open("expenses.csv", "r") as f:
    with open("expenses_processed.csv", "w") as out:
        reader = csv.reader(f)
        writer = csv.writer(out)

        for row in reader:
            requester, memo, amount = row

            if requester in GROUP_MAP:
                group_members = GROUP_MAP[requester]
                per_person_amount = int(amount) // len(group_members)

                for group_member in group_members:
                    writer.writerow([group_member, memo, per_person_amount])
            else:
                writer.writerow(row)
```

For convenience's sake we have decided to implement the `group_table`
as a hardcoded dictionary in our script. The rest of the code is pretty
straightforward: it reads rows from a CSV, and if the requester is
found in the group table, it writes out a new row for each member of
the group. Otherwise it just writes the row to the output.

This means that if we run it on a file that looks like this:

```csv
George Steinbrenner,Miscellaneous expenses,1000
Pitching Staff,New baseballs,200
Shoeless Joe Jackson,Pine tar,30
```

We'll end up with a new output file like this:

```csv
George Steinbrenner,Miscellaneous expenses,1000
Nolan Ryan,New baseballs,100
Orel Hershiser,New baseballs,100
Shoeless Joe Jackson,Pine tar,30
```

There are some decisions in here that were not specified, of course;
we write out the additional rows in arbitrary order, in the same
overall order as the original row we're replacing. And we maintain
the integer format with `//`, at the expense of not always summing
up to the original amount. These are details that may or may not be
important to the Sweathogs accounting department, but don't appear in the
specification. However: because it *is* a specification, and not just
a single work ticket, we can be confident that as long as they're not
there, they're not defined, and if we're unclear or need to drill down,
we can add them to the spec. Without an authoritative specification those
details might be assumed on the part of the product owner or developer,
or they might only be found in a previous ticket.

Finally, we can use the invariants stated in the specification as a prompt
for writing tests. Thinking in terms of invariants and propositions lends
itself very well to property-based testing; in this case we might use
the Python [hypothesis](https://hypothesis.works/) library to verify
invariants stated in the specification, like that if a requester is
found in the group table, there should be no row in the resulting text
that contains it.

## An apology for specifications

Isn't this all a bit unnecessary? Pantagruel is at its heart just
a convenient and consistent notation for putting down basic logical
statements; it isn't the sort of heavy-duty formal method where you
end up with something you can prove mechanically. And for programs that
don't need that kind of industrial-strength approach (like the one above)
what's the use of the extra step? Why go to the trouble of formalizing
a program that's so simple? What ambiguities are there, really? Is there
any other way you could have written a program like this?

Not that I can think of, no---and that's exactly the problem. In our
example I played the part of product and developer, and I thought of
the program I'd like to write, then I thought of problem that that
program would solve, then I wrote the description of the problem, then
the program. So each step of the way I naturally understood exactly what
the various parties had in mind. Not only that, but it seemed obvious;
I couldn't really come up with too many natural ways for something to
be misunderstood and executed differently.

And for many projects involving one or two people, that will still be
the case. But as soon as you get more than a few people who have to
work and communicate together, or for that matter one or two people
who have to remember what they had in mind six or 10 months ago, that
self-evidentiality becomes a liability. It's exactly the sense that
the stakeholders, product owners, and implementers perfectly understand
what is needed in a given situation that leads to the possibility for
misunderstandings, both subtle and gross.

Being forced to concretely document our understanding of a software
system gives us a chance at avoiding misunderstandings---both between
colleagues and within one's own mind. But the English or any other natural
language is ill-suited to the task; it's overly verbose and rife with
opportunities for ambiguity. Using a more formal notation like Pantagruel
to express things in terms of the basic systems of first-order logic and
and simple mathematics allows us to do exactly the work we are already
doing---articulating an unambiguous, rigorous understanding of invariants
and system properties---with a tool much better suited to the task.

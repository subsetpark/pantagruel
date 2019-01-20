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

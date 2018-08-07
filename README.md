This tool provides basic data on resource utilisation for Milton batch jobs. Its main purpose is to demonstrate how to extract data from the job monitoring database and hos the data are structured.

Job data are collected very few minutes from the `/sys/fs` and `/proc` filesystems and stored in a Postres database. See the top of `app.R` for details required to connect to the database.

Data in the `/proc` filesystem are cumulative data for each process ID in a batch job. Data will be incomplete when a process ends since that entry is also removed from the `/proc` filesysyem. This means that data for short running jobs or jobs that create and destroy process will be inaccurate. Indeed, jobs that are not present for at least to two sample intervals will not be reported. If you need data on short running jobs, use the `torquestats` tool provided by ITS.
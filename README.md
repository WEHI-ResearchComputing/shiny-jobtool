This tool provides basic data on resource utilisation for Milton batch jobs. The main purpose of this tool is to demonstrate how to extract data from the job monitoring database and how the data are structured.

Please report defects or suggest improvements to Research Computing.

Pull requests gratefully appreciated [https://github.com/WEHI-ResearchComputing/shiny-jobtool](https://github.com/WEHI-ResearchComputing/shiny-jobtool)

## Details
Job data are collected very few minutes from the `/sys/fs` and `/proc` filesystems and stored in a Postres database. See the top of `app.R` for details required to connect to the database.

Torque creates a cgroup (essentially a container) for each job. Process ids for those jobs are available in the `/sys/fs` filesystem. Resource usage data for the process ids are then read from the `/proc` filesystem. (This is where `top` and similar monitoring tools get their data.) 

CPU timing data are cumulative data for each process ID. Data will be incomplete when a process ends since that entry is also removed from the `/proc` filesystem. This means that data for short running jobs or jobs that create and destroy processes will be inaccurate. Indeed, jobs that are not present for at least to two sample intervals will not be reported. If you need data on short running jobs, use the `torquestats` tool provided by ITS.

Memory use is the value at the sample point.


# UrbanFit Dashboard

This is a real dashboard for a mock company, powered by randomly generated data to produce delightfully non-sensical insights. While the data is fake, the infrastructure is real, featuring:

- A functioning [Supabase](https://supabase.com/) backend
- A complete Shiny frontend built with `R` (and heavy use of `tidyverse` and `ggplot2`)
- Deployment to [Shinyapps.io](https://www.shinyapps.io/)

## 📊 Live App

👉 [View the UrbanFit Dashboard](https://michael-l-davies.shinyapps.io/urban_fit/)

## ⚙️ Features

- **Interactive plots** displaying fitness class signups, memberships, financials, and check-ins.
- **Data modeling** to highlight members with declining attendance.
- **Supabase integration** for persistent cloud data storage.
- **Modular UI** with a Fundamentals and Membership section.

## 🌍 Why This Stack?

The entire stack leverages **free-tier, highly scalable cloud resources**—making it an ideal solution for:

- **Solo developers** or **data scientists** building prototypes or MVPs  
- **Small businesses** seeking low-cost, cloud-hosted analytics solutions  
- **Educators** and **students** experimenting with full-stack data apps  
- **Startups** needing lean infrastructure with minimal overhead

## 🚀 Tech Stack

- **R / Shiny**
- **Supabase**
- **ggplot2** + **ggthemes** (Tableau palette)
- **DT** for searchable data tables
- **renv** for reproducible environments

## 📂 Repository Structure

- `/data/` – simulated CSV data (if included)
- `/www/` – custom styles or images
- `app.R` – the main Shiny application
- `renv.lock` – package version tracking

---

> Made with 🧠 and 🏋️‍♂️ by [Michael L. Davies](https://github.com/MLDavies)

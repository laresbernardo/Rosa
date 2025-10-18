---
title: "Rosa Documentation"
author: "Bernardo Lares"
output:
  html_document:
    toc: true
    toc_depth: 2
    toc_float: true
    df_print: paged
    md_extensions: +raw_html
---

<a href='https://github.com/laresbernardo/Rosa' target="_blank">
  <img src='RosaLogo.png' style='float: right; height: 160px;' />
</a>

### Content

1.  Introduction
2.  MMM Projects
3.  Dataset Building and Inputs
4.  Configurations
5.  Execution
6.  Results & Outputs
7.  Budget Allocator
8.  Business Engagement
9.  Other Functionalities
10. Resources

-----

# 1\. Introduction

## Marketing Mix Model Overview

  - A **Marketing Mix Model (MMM)** is a data-driven statistical analysis that quantifies the **incremental sales impact and ROI** of **marketing and non-marketing activities.** It aims to understand the **attribution** of different factors to the overall performance of a product or service.
  - It is used to understand **how to optimally allocate marketing budget** across promotional channels, products, and regions, allowing for flexible **scenario simulations** based on past performance and businesses’ needs and constraints.

### How does it work? Regardless of the tool, provider, or approach:

**1º Data Collection**
Collect external factors and marketing inputs
Collect impression and business metrics

**2º Data Treatment**
Analyze data collection and clean the data

**3º Define Statistical Model**
Model definition and statistical regression analysis to minimize model error

**4º Business Insights**
Data outputs and business insights
Budget optimization results

**Expected Results**: The dependent variable (i.e., sales) is explained by independent variables (i.e., spend channels), leading to optimal budget allocation and ROI.

### Main benefits of adopting MMM in your business

  - **Discover**: Not all learnings will come from an MMM. While gathering the data, analyze it to better understand the distribution of spends for promoting your brand, debate the status quo and best practices, and discover inconsistencies or missing information.
  - **Measure**: Measure the true impact of your promotional efforts and calculate the **ROAS (Return On Advertisement Spend)** or **CPA (Cost per Acquisition)** for your brand and for each promotional channel in your portfolio.
  - **Optimize**: Allocate your (limited) resources in the most optimal way. Find answers to business scenario questions you may have and calculate the optimal budget allocation and theoretical impact.

### Objectives and Expectations for MMMs

**What kind of questions can an MMM help answer?**

Make the most of an MMM by setting clear objectives and by having realistic expectations. Be sure you know what success looks like, and check that your team’s abilities and tools match your ambitions.

  - How many extra sales do we get back from what we spend on each promotional channel?
  - How do competitive and external factors influence our total sales?
  - What’s the smartest way to allocate our marketing budget across available channels?
  - How would sales be impacted if we modify X or Y in our marketing plan?
  - What is the point of diminishing returns for each of my promotional channels?
  - ...

### What an MMM requires to be successful?

Mastering MMM is all about **Education to Empower**, **Data to Drive**, and **Decisions to Deliver**.

#### Education

  - **Scope**: Understand what an MMM can and cannot answer for your business/brand.
  - **Theory**: Spend time to understand the fundamentals of the methodology.
  - **Business**: Always consider the proper and right stakeholders for every step.

#### Data

  - **Quality**: Ensure the data used is clean and accurate. Rubbish in, rubbish out\!
  - **Relevance**: All data included must be relevant within the business coverage.
  - **Availability**: Historical data must be as updated and granular as possible.

#### Decisions

  - **Actionable Insights**: Use the results to inform stakeholders and decision-makers.
  - **Implementation**: Apply key learnings to optimize marketing efforts.
  - **Measure**: Continuously evaluate the impact of the decisions taken.

### How does an end-to-end MMM look like?

  - Collect relevant marketing and non-marketing factors that may drive sales
  - Connect marketing and other activities to business performance using advanced statistical models
  - Quantify the effectiveness and sales impact of all activities
  - Run simulations and optimizations to adjust marketing spend for improved results

## What is Rosa?

Rosa is an **MMM interface** built to train models, understand impact, and optimize allocation. It is a powerful tool developed by Bernardo Lares and designed to run Marketing Mix Models on a global scale. Users can seamlessly manage end-to-end projects, regardless of their attribution coding expertise. Rosa enables in-depth analysis, addressing crucial business queries on attribution, incrementality, and budget optimization.

### Rosa allows for a friendly, scalable MMM approach

Rosa is a standalone, sensitive-data safe, and validated tool, powered by Meta’s open-sourced **Robyn**. It is designed bottoms-up for business experts to seamlessly run MMM end-to-end.

  - **User-Friendly**: Designed for a broad audience, including non-technical business and marketing experts, as well as data scientists.
  - **Secure & Private**: A standalone solution that can be run locally or on a secure cloud, ensuring no data leaks. You own the code and deployment.
  - **Proven Performance**: Validated across multiple international markets and diverse brands.
  - **AI-Powered**: Leverages AI tools to assist and automate processes, which help reduce human bias and improve delivery times (e.g., AI Assistant trained with documentation, Dataset AI to interact with the dataset, ML Algorithms).
  - **Comprehensive Workflow**: End-to-end documented workflow, covering everything from data preparation to insights and budget allocation.
  - **Robyn-Powered**: Built upon and fully interoperable with Robyn as its core engine, enhanced with unique additional features. Fully developed by Robyn’s main developer.
  - **Scenario Simulation**: Provides the ability to simulate various scenarios and answer real-world business questions effectively.
  - **Automated Reporting**: Generates editable PowerPoint reports, providing actionable and simplified outputs.
  - **Extended Functionality**: Includes additional features such as logs, model recreation, model refreshing, exporting, AI chat capabilities, and others.

### Differences between Rosa and other MMM Tools

#### Rosa as a Tool

  - Built for **non-technical business experts**.
  - Private standalone solution run locally or on our cloud.
  - Automatic processes using ML to **mitigate bias**.
  - Budget allocator with **maximize response and target efficiency real-world scenarios**.
  - Highly computation resources required on the back-end (runs in private cloud but runs locally too).
  - Powered by **Robyn**, an open-source, top MMM code.
  - Validated during a pilot across 7 EU markets.
  - Can be **calibrated with experimental data**.
  - Uses **spend values** to model and calculates ROAS directly with no transformations or proxies.
  - Manual data gathering and tailored dataset build.
  - Panel data (i.e., Geographic/DMAs) not enabled.

#### Common across other MMM tools

  - Oriented for technical MMM and stats experts.
  - Shares sensitive sales data with (expensive) providers.
  - Biased (i.e., manual selection of hyperparameters and seasonality variables).
  - Basic budget allocator or media mix optimization scenarios available with no business constraints.
  - Highly time consuming and expertise required.
  - In-house or traditional approach, complex code, not validated externally.
  - Manual restrictions based on hypotheses.
  - Uses **exposure metrics** to model and calculates ROAS using weights provided by marketing teams.
  - Some solutions offer API connections for raw data.
  - Some solutions may show results per groups.

### Rosa is a Front-end Tool powered by Robyn

Robyn is an open-source MMM solution that helps automate previously handmade steps by minimizing bias through the usage of ML techniques.

## MMM Alternative Solutions

| **Category** | **Rosa** | **Cassandra** | **Keen** | **Meta Robyn** | **Google Meridian** |
| :---| :---| :---| :---| :---| :---|
| **Interface** | User-friendly app for business + tech users | No-code web UI for marketers | Full UI with dashboards and planners | No UI (code-only, R-based) | Code-based (Python notebooks) |
| **Deployment & Security** | Fully local or private cloud (no data leaks) | Cloud-only SaaS | SaaS-only | Local or cloud (open-source) | Runs on GCP or local, data stays controlled |
| **Performance Proof** | Validated across brands and markets | Case studies with ROI improvements | Proven with client outcomes (e.g. revenue lift) | Dependent on user expertise | Tested at scale (Google case studies, e.g., Finder) |
| **AI & Automation** | AI Assistant + ML-powered automations | ML-powered modeling + optimizers | Adaptive Bayesian MMM, real-time modeling | Manual model tuning required | Bayesian + experimental priors, no UI automation |
| **End-to-End Workflow** | Complete MMM flow incl. reporting, model updates, etc. | Guided UI from data upload to budget allocator | Automated data flows, modeling, planning UI | Manual scripting in R | Modular templates, you build pipeline |
| **Modeling Engine** | Robyn-based MMM with enhancements | Proprietary ML engine | Adaptive Bayesian MMM | Robyn's open-source MMM | Google open-source Bayesian MMM |
| **Scenario Simulation** | Built-in scenario tools (what-if, ROAS, optimizations) | Easy scenario testing and budget optimizer | Rich planner: simulate spend by week/season | Manual adjustments required | Custom scripts or dashboard visualizations |
| **Reporting** | Auto-generated PowerPoint reports and dashboards | On-screen charts, exportable data | Insight dashboards (UI-based) | CSVs/plots only; user builds reports manually | Outputs tables/metrics |
| **Extra Features** | AI chat, logs, refresh, code tweaks, calibration, branded customization | Incrementality testing, simple setup | Demand planning, retail, 500+ data connectors | None built-in; just modeling | Google Ads connectors, reach/frequency integration |
| **Target Users** | **Business teams** & data scientists | Marketers without technical skills | Cross-functional teams (marketing + finance) | Data science professionals | Advanced data teams & analysts |
| **Support & Consulting** | 10h included, custom support | Community Slack + onboarding | Support included with SaaS subscription | No official support | Open-source; community support |
| **Pricing Model** | One-time fee (exclusive or non-exclusive), includes support | Free (may change later) | Enterprise SaaS (trial available) | Free open-source | Free open-source (cloud infra may cost) |

## Scaling MMMs

One of the largest Media Agencies (Publicis) scaling MMM through a similar approach

```
“...we aim to empower marketers by supporting them from inception and ideation to execution and measuring impact.”
```

Sep 30th, 2024: Read more [here](https://adtechtoday.com/publicis-media-launches-marketing-mix-modeling-tool-markriti/) and [here](https://bestmediainfo.com/mediainfo/mediainfo-marketing/publicis-media-india-launches-machine-learning-based-marketing-mix-modeling-mmm-tool-markriti-7148339).

-----

# 2\. MMM Projects

**Stakeholders, expectations, key dates, sessions**

## Timeline Proposed

Plan Overview for first-time adoption. Timings are estimated assuming project prioritization.

### (2 weeks)

1.  Kick-Off

  - **MMM Introduction & Training**
  - Project **scope** and **objectives**
  - Define **roles and responsibilities**
  - Align on **key milestones**
  - Confirm guiding and **operating principles**
  - **Hypotheses** building

### (14-16 weeks)

2.  Data Gathering & Creation

  - Define **timeframe** of data
  - Identification of **variables**
  - **Collect** required raw data
  - **Data cleansing** for completeness, accuracy, reasonability, and usability
  - **Data engineering**
  - Create the modeling **dataset**

3.  Modeling Journey:

  - **Map** our dataset variables to run the MMM
  - Run several configurations (Rosa) until a **good model fit** and business match is achieved

4.  Results Read-Out

  - Results **analysis**
  - **Select final model** by brand and market

### It’s a journey\!

5.  Optimization & Follow Up

  - **Assess actions/follow-ups, learnings** e.g.:
     - Guiding strategic focus for the business
     - Assessing to set the right total promotional budget
     - Optimizing channel-level budget allocation
     - Improving in-channel activity-level performance
  - Local **decision making**

**The first MMM run requires a strong focus on Data preparation.**

Business engagement is a key step for a continuous foundation for the future. Leverage key stakeholders and experts in every brand and market.

  - **1. Data** exploration, gathering, and creation: **50% time**
     - Define timeframe of data
     - Identification of variables
     - Collect required raw data
     - Data cleansing (for e.g., completeness, accuracy, usability)
     - Create modeling dataset
     - Map dataset variables to run the MMM in Rosa

  - **2. Modelling** journey and final model selection: **20% time**
     - MMM business hypotheses building
     - Learn the use and features of the Rosa tool
     - Run parameter configurations in Rosa until selecting a model with statistical fit and business match
     - Perform iterations on datasets based on modeling outputs (loop)

  - **3. Business** engagement and **results** read-outs: **30% time**
     - Results deep dive analysis
     - Sign-off the final MMM model
     - Validate total promotional budget
     - Assess budget allocator outputs and take actions to optimize channel-level budget allocation
     - Improve in-channel activity-level performance

## Expectations

### Expectations on Users

**Learn**: You won’t be an MMM expert, probably ever. But you will understand enough so that you can take relevant decisions on inputs, outputs, and data-driven decisions.

**Lead**: You will lead and own the Project end-to-end:

  - Gather relevant data
  - Analyze and clean the data
  - Input and map the variables
  - Run the models
  - Select the right model
  - Run Budget Allocator scenarios
  - Take action\!

**Leverage**: Gather the right people to help you succeed\! Make sure your team's skills are up to date and fit well with the technology you have. Ideally, have at least 2 people who are good at each thing you need, so they can work well together.

### Expectations on Local Champions

**Communicate**: Ensure that all stakeholders are kept informed and engaged with timely updates and essential information about your LOC’s progress, changes, and milestones.

**Coordinate**: Organize and oversee meetings, manage schedules, and ensure that all team members are aligned and working towards the LOC’s objectives. Especially, managing local governance and **SteerCo**.

**Champion**: Advocate for MMM adoption and success by motivating the team, pushing for deliverables, and addressing any obstacles that may hinder progress. You must be seen as a methodology expert given you will support implementation.

## Recommended Sessions

Recommended training content and tasks per session:

| Training Sessions | Content covered | Specific Asks after Sessions |
| :---| :---| :---|
| **\#0: Intro & WoW** | Quick MMM intro, Stakeholders, Expectations, Timeline, Key dates, Sessions, Success. | Align expectations and connect with your brand/market stakeholders. |
| **\#1: MMM Concepts & Rosa Introductions** | Incrementality, Attribution. What is Rosa? Why have we built Rosa? What does Rosa cover during the project? MMM equation. Quote. | Set up meeting with Business: What do you want MMM to answer? What variables do you think impact your sales? Answer brand-specific hypotheses template? |
| **\#2: Data preparation** | Minimum data requirements, variables and variable types, best practices, pitfalls, tips, and recommendations. | Think of variables to add for the brainstorming session. Collect variables data. Clean and build data set for session \#3. |
| **Brainstorming and data variable sharing between markets or brands if running in parallel** | | |
| **\#3: Model configuration** | Hands-on session with Rosa. Upload data, analysis, map your variables, understand parameters and hyperparameters settings and consequences. | Perform MMM with chosen dataset. Change dataset variables if necessary. |
| **\#4: Modelling Results** | Execute, select models, interpret one-pager results. | Select best models and discuss with Business best choice. Validate using initial hypotheses template. |
| **\#5: Budget Allocator** | Based on selected models, understand how to set up several real-life scenarios to answer budget optimization and re-allocation. | Build scenarios for budget allocator. Share with business. Get actionable insights from BUH. |
| **\#6: Business Engagement** | Templates, learnings, prepare read-outs report and presentation format, next steps. | Prepare business-oriented deck to read-out insights and recommendations to leadership stakeholders. Share/store final deck & selected model’s JSON file. |

-----

# 3\. Dataset Building and Inputs

**Building the dataset for an MMM**

### Data to Include

  - **Inference**: We are interested in inference, and we need variance. Predictive models' rules are somewhat different.
  - **Assumptions**: There can be no causal inference without assumptions.
  - **Quantity**: Adding more variables to a regression often makes inference worse.
  - **Independent**: For the model to be actionable, inputs should make independent causal sense.

## Hypotheses Template

To be able to answer our main business questions, we must first define them and write down our hypotheses

- Data selection and modeling require strong business and marketing knowledge.
- Helps narrow the bridge between business and technical, senior and junior stakeholders.
- Aligns and sets up the true priorities to be answered.
- Sets expectations, finding common ground and reducing friction between teams and read-outs.
- Plan MMM timeline to deliver insights in a timely manner when budgeting decisions occur. 

Answering the hypotheses questions should be a prerequisite to building every MMM. It is essential for business to answer to the best of their knowledge and share with the team.

> Read more hypotheses template: [Stop Building MMMs Without Business-Backed Hypotheses](https://medium.com/@laresbernardo/stop-building-mmms-without-business-backed-hypotheses-0e8a9fa9a375).

**IMPORTANT: Double check and agree with your local teams.**

### Data Hypotheses

How should my data look like to answer my business questions? Based on business needs and hypotheses, the dataset will be adapted to different formats and structures.

| Question/Task | Details |
| :--- | :--- |
| What do you want your MMM to answer for your brand? | Prioritize & sort respectively. Not all questions can be answered in a single MMM. |
| When was your brand launched? | Validate if there's enough data. Check minimum requirements. |
| What promotional (paid & organic) channels has your brand had in the last years? | Add relevant paid and organic variables. |
| What spends should be excluded for compliance, quality, relevance | What percentage of A\&P or promotional spend was excluded? |
| What internal, external or any other kind of activities may influence your brand sales? | Add relevant contextual variables. |
| Are there any peaks and valleys in your sales? Are they expected? | Validate data. |
| What data sources will you be using (i.e. local, global, etc.)? | Identify data sources. |
| What do you expect to change in the environment and promotional efforts during the following months? | Map potential refreshing. |
| When do promotional budgeting decisions happen? | Sets the project timeline and refresh frequency |

### Modeling Hypotheses

How should my model look like for me to trust it? Based on business knowledge and hypotheses, the model inputs, training and selection criteria will vary.

| Question/Task | Details |
| :--- | :--- |
| What % sales would you expect to keep in 1 year if you stop all promotional channels? Why? | Helps with model selection and validation. |
| Which channels have the shortest and longest time to take effect? | Helps with model selection and validation. |
| Which channels do you think are more and less saturated? | Helps with model selection and validation. |
| What channels with lower spend do you expect to have high impact? | Helps with model selection and validation. |
| Do you have results from other measurement experiments? | Calibrate and validate the MMM with ground-truth results from incremental tests. |
| Any learnings from previous MMM that may be transferred? | Helps with model selection and validation. |

### Hypotheses to help Business-supported Decisions

Having answered hypotheses will help you with dataset building and model selection. Each of these hypotheses can be translated as a hint to make technical business-supported decisions:

- Variables selection, classification, clustering, split, granularity, etc. always aligned to main business objectives defined
- Check if there’s enough and availability or source of data for an MMM
- Check data quality and potential or unexpected anomalies and correlations
- Set up expectations on timeline, refresh frequency, modeling window, scenario planning and budget constraints
- Simply refresh or train a brand new MMM
- Define proper hyperparameters or priors inputs for your modeling
- Select model that best describes your brand based on expectations* (baseline, lag effects per channel, ranking of channels performance, confidence intervals and values, saturation, previous models, etc.)

## Variable Types

 1. **Dependent variable**: Revenue or Conversions
 
 2. **Independent variables**:
   - Spend Variables (**mandatory**)
   - Contextual Variables (**strongly recommended**)
   - Organic Variables (**strongly recommended**)
   - Exposure Metrics associated to spend variables (recommended)

## Optimization Metrics

1)  **ROAS**: Return on Advertisement Spend = revenue caused by spend variables / raw spend
2)  **CPA**: Cost per Action or Cost per Acquisition = raw spend from spend variables / number of conversions caused by them

## Linear Regression

What’s the math behind an MMM? What’s a linear regression?

$$y_i = \beta_0 + \beta_1 X_i + \text{error}$$

A linear regression aims to find the relationship between an independent and a dependent variable. The regression line can be used to predict or estimate missing values by interpolating using an equation.

### How can we measure how good the fit is?

We are trying to understand how different promotional activities (both paid and organic) and other contextual factors affect our sales. The regression formula helps **quantify the impact of each factor independently**, considering the effects of saturation and adstock. This simplified additive formula calculates the impacts to explain the total sales:

$$y = \beta_1 \cdot \text{saturation}(\text{adstock}(\text{spends})) + \beta_2 \cdot \text{saturation}(\text{adstock}(\text{organics})) + \beta_3 \cdot \text{contextuals} + \beta_4 \cdot \text{features} + \text{error}$$

### Constant Values:

  - **Sales ($y$)**: Outcome we want to explain.
  - **Coefficient ($\beta$)**: Weighted constant value for each factor.
  - **Spend**: Amount spent on a specific marketing activity.
  - **Organic**: Marketing activity with no direct spend.
  - **Contextual**: Other external variables to help explain $y$.
  - **Features**: Intercept, trend, seasonality, etc. (automatic)
  - **Error**: Variation not explained by the model.

### Transformers (using hyperparameters):

(Covered in following sections with more detail)

  - **Saturation**: Diminishing returns of marketing spend.
  - **Adstock**: Carryover effect of past marketing efforts.

**R-squared** ($\mathbf{R^2}$): Indicates the proportion of the variance in the dependent variable ($y$) that is predictable from the independent variable ($x$). An $R^2$ value closer to $1$ means the model explains a large portion of the variance, while a value closer to $0$ means it explains very little. In MMM, we will aim for $\mathbf{0.8}$ or $\mathbf{80\%}$ or higher.

## Minimum Requirements

  - **Rows**: For monthly data, a **3-year minimum** is recommended, depending on how well the model can replicate the time series. For weekly data, at least **1.5 years**. For daily data, a minimum of **6 months** (more is recommended).
  - **Columns**: It's recommended to have at least **10 times as many rows per column** ($10:1$ relationship).
  - **KPI**: **1 dependent variable** (revenue or sales, purchases, dose uptake, etc.) per model. Values should be as close to the point of prescription as possible.
  - **Paid channels**: Minimum **2 promotional paid channels** variables (each a different column).
     - Use **actual total spend**, not budget.
     - For events, use the spend on the date of the event, not the invoice date.
  - **Exposure metrics**: Impressions/activities/etc. data is generally considered beneficial to analyze effectiveness and distribute costs.
  - **Context**: Consider including any additional and relevant information (numerical or categorical values): promotion periods, product launches, competitor activity, indications approved, price changes, etc.
  - **Rules of thumb**:
     - If channel spend represents less than 5%, it **may** be grouped with other small channels.
     - Avoid using promotional variables that contain $>80\%$ zero values or constant values.

## Dataset Building

### How does an MMM dataset look like?

  - Each variable is added as **separate columns** (e.g., date, sales, etc.)
  - Each row should be a separate day/week/month and should be in “**YYYY-MM-DD**” format.
  - **1 dependent variable** (e.g., revenue, volume, patients, prescriptions).
  - If there is no activity for a specific variable for a specific week, input **0s** (missing values are not allowed).
  - For aesthetic reasons, avoid very long variable names to avoid cluttering labels in visualizations.
  - It could help to finish each label with **\_S** (spend vars), **\_O** (organic vars), **\_E** (exposure vars), to avoid mixing them.

### Standard Data Variables Taxonomy

End your variable names with the **type of promotional data** it contains:

  - "**\_S**" for spend channels
  - "**\_O**" for organic channels
  - "**\_E**" for exposure or impression metrics available (associated with a spend channel)
  - "**\_C**" for contextual variables or, if no ending taxonomy, the variable can be assumed as contextual too

### Data Collection and Cleaning Takes Time

**Rubbish in, rubbish out!** Poor data quality means unreliable output. The model needs media variables that reflect your promotional pressure on your clients. Take your time for data collection and cleaning.

Some typical pitfalls & watch-outs:

- **Distribution**: Dividing monthly data by 4 to create weekly data creates "stairs" that do not reflect reality. Try finding other proxies that reflect the media pressure over time (e.g., visits, calls, Google Trends). (More details in coming slides)
- **Positives**: Spend and contextual variables must **only contain positive values**. Check if negative values show up!
- **Gaps**: Periods of 0s followed by a peak, then drops quickly again. This might indicate missing measurement in the 0-periods and summing everything up on a posterior day. This usually doesn’t reflect reality and suggests a gap in time rather than an actual peak.
- **Grouping**: Wrong data aggregation due to wrong or non-conventional naming for campaigns or spends. Check if different labels refer to the same campaign, or vice versa.
- **Sources**: If you are comparing granularities (i.e., weekly vs. monthly data), try extracting data independently from its sources instead of calculating.
- **Missing data**: Missing spends/accounts, especially when multiple sources, agencies, or providers are involved.
- **Relevance**: Is the spend really a promotional spend? Does it **directly** push your sales?
- **Decimals**: Some countries use commas and points opposite ways (e.g., `1.000,00` vs. `1,000.00`). Ensure consistency.
- **Dates format**: A typical case is swapping month and days.
- **Currency**: Are all spend values in the same currency? Is it in cents or dollars/euros/pounds?

Before you start modeling, don’t forget to:

- **Visualize**: Chart your data and check that it aligns with expectations, and consider asking other stakeholders to review.
- **Validate**: Make sure general trends (YoY% or Quarterly%) align with business expectations and internal reports.
- **Understand**: Question your data providers. How has the provider, your analytics team, or the agency collected the data?
- **Check**: **Triple check your data BEFORE** you start modeling to be fully confident that your data inputs are accurate.

### Weighting Aggregated Data to More Granular Levels

**Problem**: I have yearly spend data but need to weight it down to monthly spend data.

**What should I do?**

For this exercise: total spend on salaries for a year was **€1000 = salaries**.

- **Not ideal**: Divide `salaries` into 12 equal parts per month ($\text{€}1000 / 12 \approx \text{€}83.33$).
- **Recommended**: Use an exposure variable to weight spends across months.
    - We have a variable called $\text{ints}$ for the number of interactions with HCPs.
    - Each month's spend could be calculated as: $\text{salaries} \times (\text{ints} / \sum(\text{ints}))$
    - Keep in mind this calculation should be done **separately per year**.
    - The same applies for weekly granularity if you are splitting to weekly data instead of monthly.
    - Double check the sum of the 12 months' `salaries` is the same as the total year `salaries` ($\text{€}1000$).
- **Other examples:** Tenders' active months by region weighted by population, event spends weighted by attendance, or sales force costs weighted by the number of reps working per period.

**So how does MMM deal with this?**

- The more variance and information the model gets, the better it will be able to fit.

### Distributing Large Costs Based on MMM’s Main Objectives

**Problem**: I want to measure the impact on sales per **Segment** or **Type** groups.

**What should I do?**

To be able to answer specific business questions agreed upon with stakeholders, and before starting to model, you need to gather the data and build the dataset correctly. Align your MMM’s objectives with the data available by distributing costs properly across groups if needed.

Example of Costs Coverage: Not all costs are promotional nor have a direct impact over sales.

- **Include:** Base salaries and bonuses, and T&E (Travel and Entertainment).
- **Exclude:** Internal meetings, cycle meetings, and trainings.

Types of interactions: Split selling field costs based on the main objectives you want your MMM to help answer:

- **Category**: General, Specialist, etc.
- **Segment**: A, B, C, X, etc.
- **Type**: F2F, Virtual Call, Short Call, Item Drop off, etc.

### Distributing Sales Force Costs Based on Efforts per Activity

**Problem**: I want to measure the impact on sales per **Selling Field promotional activity**.

**What should I do?**

Usually, we don't have a direct cost per activity (CPA) or cost per exposure metric (CPE) available in our data. To properly allocate costs associated with Selling Field promotional activities, we first need to measure how much time or effort they spend per activity and then allocate costs.

1. Register the efforts per activity in a **standardized** format based on knowledge and feedback.
    - **Note**: Activities available in the tool are aligned with activities in local procurement tools per market/brand.
    - **Example**: Weekly (40h) average: Non-promotion 8h, F2F 6h, 1:1 emails 2h, etc.
2. Query values for the latest available data points per Market/Brand.
3. Calculate activity costs using the efforts as weights to split SF costs as separate channels.
    - **Example**: In 2024, SF spend was 100. This is distributed using the weights (20% effort in F2F calls, so 20; 15% effort in emails, so 15; ...). Then, based on the monthly number of calls, emails, etc. (**exposure metrics**) we distribute costs so that they sum each of the activities’ totals, leveraging the variance of their data.
    - If you have more than one log for your brand/market, you can use different weights across different dates.

### MMM per Region or Any Other Group (Panel Data)

**Problem**: I want to understand the **separate impact of each group** (region, indication, etc.) over my total sales. **Note**: This type of dataset is called **"panel data,"** and you must be able to split ALL data cleanly into each category (no attribution rules should be applied).

**What should I do?**

1. Aggregate groups into a single channel (i.e., sum all regions into a single market).
    1. Specific channel aggregated into a few variables (i.e., for event spend, split into Region A spend, Region B spend, and the rest of regions' spends into a single Region C spend).
2. Run **independent MMMs** per group (i.e., one MMM for each indication).

**So how does MMM deal with this?**

- Panel data for hierarchical modeling is **not enabled** through Rosa.
- We will always have the following data requirement limitations:
    1. Each row must contain a single date.
    2. The **10:1 row to column rule of thumb** (i.e., 3 years = 36 months $\rightarrow$ 4 paid variables).
    3. The smaller the effect over total sales, the harder it will be to measure.

**Recommendation**: Always have a national level model too as a reference or benchmark.

### Taking Inflation and Currency Exchange Rates into Account in MMM

**Problem**: Prices and costs are fluctuating due to high inflation or volatile exchange rates in my market. This (usually increasing) trend affects a portion of our sales and should be considered.

**What should I do?**

1. Incorporate currency exchange rate **OR** inflation as a contextual variable in the MMM.
2. Include only 1 of the two (most probably) highly correlated variables. Decide which variable affects costs and sales more significantly and include that one in the model. Typically, **inflation** tends to have a more direct impact on both costs and sales.
3. Transforming all spends and sales for inflation may be considered as an alternative if needed.

**So how does MMM deal with this?**

- To accurately capture both effects, we should add the information as contextual variables. It is not correct to assume sales are only going up if there is a big trend in inflation.
- If both variables are strongly correlated (c. $90\%$), include only one to avoid **multicollinearity**.

## Types of MMM Flows

There are 4 main flows you can follow:

**1. Build a model from scratch:**
Upload the data to the tool, set up the configurations, run the iterations, and go through the model selection process.

**2. Re-create a pre-trained model:**
Upload previously trained models you already trained and exported.

**3. Refresh model with new data:**
Upload a new CSV with old + new data and the original JSON file of your model to retrain without selecting a new model.

**4. Compare multiple models:**
Upload JSON files or select a project.

## Setup Dataset

**1. Upload a CSV or JSON with data**
For CSV, make sure to first check if it’s a comma/semicolon separated file and the format of decimals, so it uploads correctly.

**2. Just want to test the tool**
If you haven’t gathered your data yet, you can load **dummy data** to the tool to start playing with it.

**3. Check the data**
Review the data you’ve uploaded before you continue. Use the 3 buttons on the top to check stats, outliers, and correlations. Remember, you can only guarantee successful and trustworthy results if you provide clean representative data.

## Calibration

Have you run incrementality experiments? Use them to calibrate (Experimental).

Randomized Controlled Trials (RCTs) are the established gold standard to infer causality in science. With these results, we can help Rosa minimize the difference between predicted effect and causal effect.

When we provide these data points, a third objective is added to the algorithm: **MAPE** (Mean Absolute Percentage Error).

Read more about calibration [here](https://facebookexperimental.github.io/Robyn/docs/features/#calibration-with-causal-experiments).

## Check Your Data

Check data distribution and missing values.

**1. Type of data**
Each column will be either numerical or categorical. Were they detected correctly?

**2. Plot overview**
Check the distribution for numerical values. Does the scale make sense? For categorical values, are there too many different categories? Is the number of categories shown expected?

**3. Missing**
Are there any missing values in your columns? If there are, please fix and re-upload your data.

**4. Mean/Median/Standard Deviation**
Do these summarized metrics make sense? If you found the plot overview distribution sensible, these will likely make sense too.

**5. (Optional) Dataset AI**
Interact with the Dataset AI option to ask questions about the data or filter/sort/create variables.

## Dataset AI

There's an AI tool that allows you to **interact with the dataset**. You can either ask it about the data (i.e. how many rows or columns, average of all spend variables, etc) or interact with the data (i.e. filter data since X, add a new column with this copy/paste data from Excel or aggregate two columns, etc.).

## Outliers

Check for outliers in your data to avoid the following issues.

By conducting an outlier study, we ensure that the model is based on the most representative and clean data possible, improving the reliability of the insights and final recommendations.

- **Data Quality Assurance**: Outliers can indicate data entry errors or inconsistencies in data collection, which need to be corrected to ensure accurate analysis. Identifying them helps validate the data quality.
- **Understanding the Data**: Analyze to understand the data distribution and whether extreme values are part of the variation or anomalies.
- **Strategic Decision Making**: Outliers can affect the estimated impact of promotional activities, leading to suboptimal allocation of resources.
- **Prevention of Overfitting**: Models that "learn" from outliers may overfit the data, capturing noise rather than the underlying relationship.

## Overfitting? Underfitting?

**Overfitting** happens when a model learns the details and noise in the training data to the extent that it performs well on the training data but poorly on new, unseen data. We want to avoid overfitting, especially when extrapolating the data.

**Example**: Think of overfitting like memorizing the answers to a specific set of test questions instead of understanding the underlying concepts. You might do well on that specific test but struggle with any new questions on the same topic.

## Multicollinearity

Check the correlations and avoid multicollinearity.

Reviewing this chart can help decide which variables to include in the final model and identify potential double counting.

- **Correlation Analysis:** Analyzing correlations between different variables helps determine their relationships. Useful for understanding how variables influence each other. Also, helps identify patterns and potential associations.
- **Multicollinearity:** Occurs when independent variables are highly correlated with each other. It is problematic for regression models, because it makes it challenging to assess the impact of individual independent variables.
- **Dependent-Independent Variable Correlation:** Examining the correlation between each independent variable and the dependent variable helps decide which independent variables to include in the model. Visualizes expected impacts.

-----

# 4. Configurations
**Setup Rosa to run your MMM**

### Recommendations:

1. Write down all your questions.
2. Pay as much attention as possible.
3. Be eager and open to learn.
4. Take notes for your future self.
5. We (and the AI Assistant) have got your back!

## Baseline Expected

Directional baseline expected based on a brand's mature status.

For a positive market trend, the **baseline** is expected to grow during the product life cycle.

The **following references are indicative and for a positive market trend**. It may differ in your market due to other contextual conditions. If the **market has a negative trend** (i.e., reached maximum coverage/penetration rate), you can expect to see a **decreasing baseline over time**.

- You can use **market research** and **product adherence/switches** as a **reference of your baseline** if available.
- Sales that are included as part of the baseline:
    - **Non-promo spends and other market factors not included** in the model.
    - **Contextual variables** added to your dataset.
    - **Long-term promotional effects** (sales originally caused by promotional channels).

## Saturation or Diminishing Returns

What is Saturation or the Theory of Diminishing Returns?

The **theory of diminishing returns** (also called **saturation**) holds that each additional unit of advertising exposure increases the response, but at a declining rate. This is a key **marketing principle** that is reflected in MMM and Rosa as a variable transformation.

The nonlinear response of a media variable on the dependent variable (sales) can be modeled in different ways. Common approaches include transforming the media variable by the logarithm transformation, the power transformation, or any sigmoid-shape functions. These techniques allow us to use the budget allocation feature with nonlinear optimization algorithms.

Rosa implements the **Hill function** for a flexible transformation into S or C shape saturation curves. It requires 2 values to define the curve: **alpha** (C/S shapes) and **gamma** (saturation point).

## Reporting Window

Understanding Promo Channels’ Performance over Time

To observe changes in channel performance over time, a new MMM model must be built. This is because the hyperparameters (and thus saturation curves) will change with an updated strategy. Saturation curves can shift based on improved strategies, optimized campaigns, messages, creatives, etc. However, these changes will only be visible in a new model, which will have different hyperparameters.

**Reporting Window:** When filtering by date range using the reporting window, you only see the spend and performance for that specific period. This helps in locating the mean point on the channel's saturation curve. **ROAS and mROAS change due to different spending levels, not because of performance changes.**

**Example**: If the quality of my events improved over time but the spend remains the same between two different date ranges (with the same mean carryover), the performance will appear the same in both periods using the same MMM.

**Key Takeaways:**

1. Performance of promotional channels in MMM are **static within a model**.
2. To see performance change over time, a **new MMM model is necessary**.
3. Changes in ROAS and mROAS reflect the performance on **spending levels**.

## Adstock and Carryover

What is Adstock or Carryover? Is it different from Baseline?

**Baseline**: Represents the normal level of sales, customer engagement, or other key metrics that **would** occur without any marketing efforts. It's the "default" state of your metrics, reflecting ongoing trends, seasonality, and external factors unrelated to specific marketing campaigns.

Baseline is the starting point or the normal level of activity without any specific marketing efforts.

**Carryover Effect**: Refers to the lingering effects of a marketing campaign that continue to influence sales even after the campaign has ended. It's the residual impact that persists over time, showing that marketing efforts can have long-term benefits beyond the immediate campaign period.

Carryover is the extended impact of a specific marketing campaign that affects metrics beyond the initial campaign period.

Note that channels may have **carryover**, but not **baseline**.

| | BASELINE | CARRYOVER |
| :---| :---| :---|
| **Definition** | Normal activity level without marketing | Residual impact after a specific campaign ends |
| **Time Frame** | Ongoing, continuous | Occurs once the campaign starts onwards |
| **Example** | Monthly average prescriptions without campaigns | Increased prescriptions post-spend for a specific campaign |

### Aggregated Responses Explain the Total of Sales

Total sales can be explained or calculated by the sum of:

- **1. Baseline** (all those contextual variables and external factors)
- **2. Immediate** response (for each promotional paid channel)
- **3. Carryover** response (from each promotional paid channel)

## Input Settings

Setup your Inputs and Settings.

**General Note**: If you don’t understand what an input is, leave default values. Those are usually a good place to start!

**1. Local Settings:**
Load pre-saved configurations files to overwrite default values.

**2. Input Variable Mapping:**
Tell Rosa what each column in your dataset is for (no need to use them all). Leverage the plot and summary table to double-check.

**3. Modeling Settings:**
Select how many cores to use for parallel computing, set up how many iterations and trials to run, and change any advanced settings you’d like to modify.

## Mapping Variables

- **Date Column:** The column name of your monthly/weekly/daily time series variable. It must follow the format **YYYY-MM-DD**. It may be detected automatically if only one date variable is present in your dataset.
- **Dependent variable:** The name of the variable or KPI that you are modeling.
- **Dependent variable type:** Is your dependent variable **revenue** (currency) or is it **conversions** (units)? Notice that:
    - **ROAS** (Return on Advertisement Spend) will be used for "revenue."
    - **CPA** (Cost Per Action) will be used for "conversion."
- **(If “conversion” type):** What does a unit of your conversions dependent variable refer to? (i.e., doses administered, packs of 6 vaccines, etc.)
- **Currency**: Which "common currency" are all promotional spends in?
- **Paid channels** have an associated media spend that the model would return Cost per Conversion or ROAS for (Sales Force, Events, Prints, ...).
- **(If available) Exposure metrics** are associated with paid channels to calculate CPA (cost per actions) (HCP Visits, Impressions, ...).
- **Organic variables** are marketing activities without an associated ad spend (e.g., 1-to-1 email, mass email).
- **Context variables** are non-marketing factors that you want the model to consider (weather, Covid cases, promotion periods, etc.).
- **Signs inputs** (optional) force variables to be positive, negative, or leave as default (positive for promotional channels and free for contextual).
- **Categorical Variables** (optional) to specify context variables that are not numeric.
    - **Example Factors**: True/False (\*use $1/0$ instead), Rain/Snow/Clear, Promotion Type.
    - **Example Not Factors**: Temperature, Discount %, Covid Cases.
- **Offline Variables** (optional) to help shape the saturation curves to look more like a C shape instead of an S shape (alphas set to 0 and 1 – will explain later!).

## Modeling Settings

- **Iterations & Trials:** The number of models that will be created equals $\text{iterations} \times \text{trials}$. More iterations mean better convergence.
    - **Testing**: $2000-3000 \times 1$
    - **Final model**: $4000 \times 5$ (or so)
- **Modeling Window:** Set start and end dates of the modeling period. Recommended to **not start on the first date in the dataset** to gain adstock effect from previous periods. Also, the columns to rows ratio in the input data should be, at least, 10 observations to 1 independent variable. This window will determine the date range of the data period within your dataset you will be using to specifically regress the effects of media, organic, and context variables on your dependent variable. We recommend using a full dataset with a minimum of 1 year of history, as it will be used in full for the model calculation of trend, seasonality, and holidays effects. Whereas the window period will determine how much of the full data set will be used for media, organic, and context variables.
- **Time series decomposition:** Automatically splits trend, season, holiday, and weekday from the dependent variable as new variables to help the model achieve better fit.
- **Holidays country**: When Holidays is active, the country for which the holidays will be automatically added.
- **Time series decomposition components:**
    - **Trend**: Long-term and slowly evolving movement (either increasing or decreasing direction) over time. In marketing, the trend can be generated by factors that can create long-term momentum (e.g., general market growth in specific categories, gradual economic downturn, etc.).
    - **Season**: The repeating behavior that can be captured in a short-term cycle, usually yearly. For example - depending on the vertical or category, specific brands can have more sales in summer than winter.
    - **Holiday**: Holidays (data source) or other events that highly impact your dependent variable (e.g., national holiday, coronation day, etc.). We can include them for the country selected (or manually add them as additional columns in the dataset).
    - **Weekly/Monthly**: The repeating behavior that can be captured within a week. Note that this is only usable when daily data is available.

If you do not already have trend/seasonality data of your own, we would recommend you consider using **Prophet** for at least the **trend** and **seasonality** components.

## Time Series Decomposition

Automatic time series decomposition.

Classic MMM models relied on the experience of the analyst to customize trend and seasonality: Prophet has generalized calendar-based assumptions that can be used automatically for many businesses. Prophet can detect the underlying trend, seasonality, holiday, and weekday impacts, in order to improve the model accuracy. Highly recommended to use for a basic model. Seasonality/holiday data can be modeled manually: Holidays can be customized (e.g., Mega Sales Day), but we will be using generic holidays by country.

- **Time Series Validation:** Switch **ON** to split data by test, train, and validation partitions to validate the time series ($\text{rmse\_val}$ is the objective function); switch **OFF** for only train validation ($\text{nrmse\_train}$ is the objective function). Used to validate over-fitting. Not required.
    - **Train size**: When turned ON, an option to set a fixed value or a range will show up. Recommended: fixed $\mathbf{0.8}$ or flexible value range between $0.5$ and $0.8$.
- **User Penalty Factor:** In case you are having good but not good enough $R^2$ and want to test adding additional hyperparameters so the model can be more flexible, you can turn this ON. By default, we’d recommend it to be **OFF**.
- **Add Intercept:** By default, we recommend it to be **ON** so the model adds the intercept as part of the baseline.
    - **Intercept sign**: When turned ON, the default and recommended value is non-negative, but can also be unconstrained. If the intercept is negative, Rosa will drop the intercept and refit the model.

## Hyperparameters

- Rosa automatically searches for the best values that, when applied to the data, return the lowest errors and best model fit.
- Each of the ranges defined are the allowed values for the algorithm to search the best hyperparameter values from.
- Each paid channel and organic variable will have its own set of 3-4 hyperparameters depending on the selected adstock.

**Saturation:** The point at which further increases in marketing channels have diminishing or no return effect. There is a limit to how much impact a marketing activity can have on sales, and beyond that point, increasing the marketing activity will not lead to efficient sales increase.
**Hyperparameters: alpha (C/S shapes), gamma (saturation point)**

**Adstock:** The amount of residual impact that a specific channel has on consumer behavior even after the spend has stopped.

- **Geometric** is the classic one-hyperparameter adstock function.
    **Hyperparameters: theta (decay rate)**
- **Weibull** is a two-hyperparameter adstock function that allows changing the decay rate over time, as opposed to the fixed decay rate over time in Geometric adstock. It has two options, the cumulative density function **CDF** or the probability density function **PDF** (default).
    **Hyperparameters: shape (U/S shapes), scale (inflection point)**

## Adstocks

### Geometric

- The most simple assumption.
- The **theta** hyperparameter will adjust for ads that have a shorter or longer lasting impact, but the shape is fixed to exponential decay.

### Weibull CDF
- Weibull CDF still assumes peak impact on day 1, but has more flexibility in how the impact decays over time.
- Makes sense for the most common DR (Direct Response) case where most of the impact is seen on day 1.

### Weibull PDF
- Weibull PDF can also account for activity where **peak impact may not occur on day 1**.
- The classic example is automobile sales, where there is a long time between the decision to purchase and the actual purchase.
- Weibull PDF may also be better for capturing upper-funnel activity where the impact builds over time.

**Tip**: Consider fine-tuning hyperparameters only after you have a good enough model (high $R^2$).

### Parameter Configuration: Key Takeaways

- Don’t expect everything to make sense and run smoothly after the first run. It can be overwhelming!
- Don’t let perfect be the enemy of great - start by trialing different bounds.
- Do ensure you are comfortable with assumptions used for adstocks and saturation curves and are able to convey it to stakeholders in non-technical terms (**critical to get buy-in**).

## Summary: Inputs Glossary

|Parameter                              |Description                                                                                                                                                                                                                                       |Required                         |Default                                                                           |
|:--------------------------------------|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:--------------------------------|:---------------------------------------------------------------------------------|
|Input Data                             |Raw input data. Must contain date, dep_var, spend variables, organic variables, and contextual variables. No empty values. Recommended: columns to rows ratio to be >=10:1, or in other words at least 10 observations to 1 independent variable. |TRUE                             |(UPLOAD). You may use the demo data but use your own data to run a real MMM.      |
|Calibration Data                       |Experimental results for MMM calibration.                                                                                                                                                                                                         |FALSE                            |(UPLOAD)                                                                          |
|Dependent Variable                     |Name of the dependent variable we are modeling.                                                                                                                                                                                                   |TRUE                             |(SELECT variable)                                                                 |
|Dependent Variable Type (dep_var_type) |Type of dependent variable as "revenue" for sales or "conversion" for units. Impacts calculation of ROI or CPA, affecting outcome measures and model interpretation.                                                                              |TRUE                             |(SELECT type)                                                                     |
|Date variable                          |Name of date variable. Keep in mind you must provide the dates on a daily, weekly or monthly granularity, depending on how your data is available.                                                                                                |TRUE                             |(SELECT variable). If there is only one date column, it detects it automatically. |
|Paid media spends                      |Names of the paid media variables. They must contain the accrued spends per period.                                                                                                                                                               |TRUE                             |(SELECT variable(s))                                                              |
|Paid media variables                   |Names of the paid media exposure level metrics, like impressions, GRP, etc. Not required.                                                                                                                                                         |FALSE                            |(SELECT variable(s))                                                              |
|Paid media signs                       |Signs of coefficients for paid_media_vars. Must have the same length and order of paid_media_vars input if used.                                                                                                                                  |FALSE                            |"default"                                                                         |
|Organic variables                      |Names of organic variables. These are marketing efforts that do not have a spend associated to them, such as notifications, emails, etc.                                                                                                          |FALSE, but recommended           |(SELECT variable(s))                                                              |
|Organic signs                          |Signs of coefficients for organic_vars. Must have the same length and order of organic_vars input if used.                                                                                                                                        |FALSE                            |"default"                                                                         |
|Context variables                      |Names of contextual variables. These variables help the model understand external factors that are part of your baseline, such as competitors, market size, etc.                                                                                  |FALSE, but strongly  recommended |(SELECT variable(s))                                                              |
|Context signs                          |Signs of coefficients for context_vars. Must have the same length and order of context_vars input if used.                                                                                                                                        |FALSE                            |"default"                                                                         |
|Factor variables                       |Variables to be forced as factors, especially if they "look like" numerical.                                                                                                                                                                      |FALSE                            |(SELECT variable(s))                                                              |
|Units of conversion                    |When modeling conversions (units), define what each unit of conversion is: prescriptions, units sold, etc.                                                                                                                                        |FALSE                            |                                                                                  |

## Summary: Parameters Glossary

|Parameter              |Description                                                                                                                                                                                                                                     |Required                                 |Default                                                                    |Comments                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
|:----------------------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:----------------------------------------|:--------------------------------------------------------------------------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Iterations             |Number of iterations for modeling. The more iterations the more models will be built that minimize the errors. Usually around 2000-3000 would be enough.                                                                                        |TRUE                                     |3000                                                                       |Affects model accuracy; too few iterations may result in suboptimal models. Check the ts_validation.png plot to check convergence per trial. If it converges, the R2 won't be able to improve no matter how many more iterations you run.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |
|Trials                 |Number of trials for modeling. This represent different random starting points. Each trial will run the amount of iterations set.                                                                                                               |TRUE                                     |3                                                                          |Affects model robustness; 1 trial may lead to less reliable models. Use 1 when testing, maybe 3-5 trials when running the final models.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
|Cores                  |Number of cores for parallel computing.                                                                                                                                                                                                         |FALSE                                    |15                                                                         |Affects computation speed; insufficient or too many cores can slow down the process.Around 10-15 seems OK.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
|Window start and end   |Start and end dates of modeling period. These are the date that will be used to train the model. If there are date outsite this window, they will only be used to create time series decomposition variables by prophet.                        |TRUE                                     |Starts more or less at 5% of available dates. Ends on last available date. |Recommended to not start in the first date in dataset to gain adstock effect from previous periods. Also, columns to rows ratio in the input data to be >=10:1, or in other words at least 10 observations to 1 independent variable. This window will determine the date range of the data period within your dataset you will be using to specifically regress the effects of media, organic and context variables on your dependent variable. We recommend using a full dt_input dataset with a minimum of 1 year of history, as it will be used in full for the model calculation of trend, seasonality and holidays effects. Whereas the window period will determine how much of the full data set will be used for media, organic and context variables. |
|Prophet variables      |Variables for Prophet decomposition: "trend", "season", "weekday", "monthly", "holiday" or none. All these variables will be automatically created and will be part of the baseline.                                                            |FALSE, but strongly recommended          |"trend", "season", "holiday"                                               |Highly recommended to use all for daily data and "trend", "season", "holiday" for weekly and above cadence. These variables will help the model explain your baseline and will probably help you increase R2.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|Prophet signs          |Signs of coefficients for prophet_vars. Choose any of "default", "positive", "negative". Control the signs of coefficients for prophet_vars. Must have same order and same length as prophet_vars. By default, all values are set to "default". |FALSE                                    |"default"                                                                  |NA                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
|Prophet country        |Country for national holidays.                                                                                                                                                                                                                  |TRUE if holidays is used in prophet_vars |(SELECT)                                                                   |This will also help the model explain part of your baseline. Used to also set default country when reporting/exporting model.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|Time series validation |Validate the time series by splitting data into test, train, and validation partitions.                                                                                                                                                         |FALSE                                    |FALSE                                                                      |Ensures model validation; incorrect setting can lead to overfitting or poor model validation. By default, it's set to FALSE but it is a good excercise to run with TRUE and understand the overfitting of your time series. If it's overfitted, this model is not great with forecastings and data not seen by the regression. To identify overfitting, check the validation errors: if very high, then it is overfitted. When this parameter is being used, the R2 will be strongly impacted and will decrease.                                                                                                                                                                                                                                                |
|Add penalty factor     |Add penalty factor to hyperparameters. May require more iterations.                                                                                                                                                                             |FALSE                                    |FALSE                                                                      |Can add complexity to hyperparameter space; may require more iterations for convergence. Usefull to improve the models R2 a bit.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                |
|Intercept              |Should intercept(s) be fitted (TRUE) or set to zero (FALSE).                                                                                                                                                                                    |FALSE                                    |TRUE                                                                       |Affects model fit; incorrect setting can lead to biased or less accurate models. If it's a new brand or business you may set to FALSE; else, keep TRUE.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
|Intercept sign         |Sign of intercept ("non_negative" or "unconstrained"). Intercept is part of the baseline and it is a constant value.                                                                                                                            |FALSE                                    |"non_negative"                                                             |If intercept is negative, Robyn will drop intercept and refit the model. Consider changing intercept_sign to "unconstrained" when there are "context_vars" with large positive values.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          |
|Adstock                |Type of adstock function: "geometric", "weibull_cdf", or "weibull_pdf". The latter is the most flexible.                                                                                                                                        |TRUE                                     |"weibull_pdf"                                                              |Weibull adstock is a two-parametric function and thus more flexible, but takes longer time than the traditional geometric one-parametric function. CDF, or cumulative density function of the Weibull function allows changing decay rate over time in both C and S shape, while the peak value will always stay at the first period, meaning no lagged effect. PDF, or the probability density function, enables peak value occurring after the first period when shape >=1, allowing lagged effect. Check the plot adstock option to see the difference visually. By definition, given weibull is more flexible, it'll get higher R2.                                                                                                                         |
|Hyperparameters        |Hyperparameter bounds for paid and organic variables. Contains lower and upper bounds (or fixed values).                                                                                                                                        |TRUE                                     |Default values are provided but you can customize them.                    |The default values are usually goot to go, but user can change hyperparameter ranges to better reflect the channels' impacts on their businesses. Check the plot adstock option to see the impact of changing any of the hyperparameters visually. The wider these are, the wider the range the algorithm will try to find the values that lower the errors and better R2, but may be unrealistic. Customize with caution.                                                                                                                                                                                                                                                                                                                                      |
|Randomness Seed        |For reproducible results when running nevergrad.                                                                                                                                                                                                |TRUE                                     |123                                                                        |To be able to replicate results, every input should be exactly identical.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |

-----

# 5. Execution
**Run calculations and find convergence**

## Run Iterations

Each iteration will be a unique simulations MMM.

- **Run Rosa, run!**: Everything is set up to run the Robyn-powered MMM algorithm now. Go to back to Configurations if something’s off. Once you double checked everything is as it should be, run Rosa!
- **Prints and logs**: Check the logs to see what’s happening underneath. It has useful information on the methodology and results.
- **Don’t kill it**: It takes some time to run (about 5 minutes for 2000 thousand iterations and 2 trials). Please, don’t stop mid-way and let it run. Be patient.
- **Convergence**: Check out the multi-objective optimization cloud at the bottom and continue to Results if it converged.

## Convergence

**Did the algorithm converge?**

The algorithm is trying to find the right values for hyperparameters to reduce multiple errors simultaneously.

- **NRMSE:** How far away are the reality and the model’s mathematical results?
- **DECOMP.RSSD:** How far is the distribution of spends vs effect for every paid channel? Also called, business error.

If it doesn’t show that the algorithm is getting closer to the lower left corner, it hasn’t converged (or is not able to). When this happens you need more iterations, better data, or more flexibility in your parameters.

### Why so many different unique models?

Each model will have their specific:

- Coefficients (betas)
- Hyperparameters (transformers)

Which means, they will all have different:

- Errors
- Effect sizes and distributions
- Carryover per channel
- Saturation per channel
- RESULTS

But they all are trying to minimize the errors.

**Remember, the math doesn’t understand what the numbers represent, but you do!**

-----

# 6. Results & Outputs

**Select the right model, interpret results, optimize budget allocation**

### **Remember: “All models are wrong, but some are useful”.**

## Model Selection

Select the right model based on your criteria. Out of the thousands of models built, Rosa automatically selects the top ~100 performers, clusters them into alike groups and ranks/sorts them based on your defined criteria.

- **R2:** How much of your dependent variable is the model able to accurately explain?
- **Performance**: High ROAS for revenue or Low CPA for conversions.
- **Non-Zeroes**: Percentage of non-zero paid channels variables; the more the better.
- **Models in Cluster**: the more models per cluster there are, narrower CIs (better)
- **Baseline Distance to reference:** compared with an expected baseline value (for non-promotional channels sales or L4 Baseline).
- **DECOMP.RSSD error:** the smaller, the more conservative spend/impact distribution.
- **Distance to cluster’s mean**: the closer the model’s paid channels performance is to the rest of the models, the highest our confidence.

**Note:** The more stars (****), the better the model is ranked based on criteria

Study the best models’ one-pagers to pick the best one. The models are sorted by the best to the worst based on your criteria. But not all models will make business sense. We must zoom in on the models, compare them, and select the one that better explains the business.

If none of the models are good enough, you must re-run the iterations, testing any of the following (order strongly recommended):

1. Model Selection Criteria
2. Data (add, check quality, new variables, ...)
3. Variable selection (add / remove)
4. Parameters (more iterations, more trials, model window, different adstock, time-series validation on/off, enable penalization, forcing signs,...)
5. Hyperparameters types and ranges

## Selection guidance

The following recommendations and rules of thumb will help you select the best model out of all the options.

|                      | Item                           | Metric                                                                               | Must Have | Good to Have | Comments                                                                                                                                                                        |
| :------------------- | :----------------------------- | :----------------------------------------------------------------------------------- | :-------- | :----------- | :------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| Statistical Validation | R2                             | 0.8 - 0.9                                                                              | ✅        | ☐            | At least 0.8 is a must have. Above 0.9 can be OK but not desirable to get too close to 1.                                                                                   |
|                        | Number of clusters             | 3 - 6                                                                               | ☐         | ✅           | More important to have a cluster with lots of models, but still relevant                                                                                                        |
|                        | Number of models in cluster    | +30                                                                                | ✅        | ☐            | The more the better. Keep in mind there are a bit more than 100 models in total being considered                                                                                |
|                        | Confidence Intervals           | Narrow Intervals for high spend channels (high confidence). Less strict for low volume channels | ☐         | ✅           |                                                                                                                                                                                 |
|                        | Business and Prediction error convergence | Convergence Achieved (visually) for both when running iterations                     | ✅        | ☐            | Make sure to select a model that is in the lower left side to avoid good fit but crazy results or viceversa                                                                      |
|                        | Distance to cluster mean       |                                                                                      | ☐         | ✅           | The larger the spend, the more relevant the distance to mean                                                                                                                    |
| Business Validation   | Baseline %                     | Aligned to Business expectation -Which Baseline level represents reality the most - L4? | ✅        | ☐            | Aligned with the hypotheses template values or ranges                                                                                                                           |
|                       | Channel response decomposition | Do the Channel contributions MOSTLY make business sense? I.e. Low volume channels should not have disproportionately higher impact | ✅        | ☐            | Aligned with the hypotheses template values or ranges                                                                                                                           |
|                       | Adstock/Carryover effects      | Does the Adstock at a channel level MOSTLY make sense?                               | ✅        | ☐            | Aligned with the hypotheses template values or ranges                                                                                                                           |
|                       | Saturation                     | Does the Saturation at a channel level MOSTLY make sense?                            | ✅        | ☐            | Aligned with the hypotheses template values or ranges                                                                                                                           |

## Reporting Window

Using the reporting window date range for more granular results. Inspect the one-pager’s results at a more granular level by modifying the reporting window date range filter. By default, the reporting window range will contain the date range that contains all the available data.

Individual one-pager plots will adapt to reflect their reporting date range results and will be marked with a star. The adapted plots are:

- Response Decomposition
- Revenue Decomposition
- Share of Spend/Share/ROAS
- Immediate vs Carryover
- Fitted vs Residuals (and MAPE error)

## Rosa Model One-Pager

The one=pager contains several sections, each with useful information. The main ones are:

1. Model’s information
2. Response decomposition
3. Actual vs Predicted with splits
4. Share of Spends vs Share of Effect
5. In-cluster ROI with confidence intervals
6. Adstock Decay Rate
7. Immediate vs Carryover per Channel
8. Response Curves
9. Fitted vs Residuals
10. Robyn and R versions

## One-Pager's Information

- Model ID is composed by trial, iteration and cores used when the model was created. Unique ID for every model.
- R-squared (R2) is a statistical measure that represents the proportion of the variance for a dependent variable that's explained by an independent variable or variables in a regression model. Responds: how good a fit is?
- NRMSE (Normalized Root Mean Square Error) aims to minimize the model’s prediction error (difference between time series and prediction)
- DECOMP.RSSD (decomposition root-sum-square distance). The distance accounts for a relationship between spend share and a channel’s coefficient decomposition share. If the distance is too far, its result can be too unrealistic - e.g. media activity with the smallest spending gets the largest effect.
- MAPE (Mean Absolute Percentage Error) to minimize the error between incrementality measured with experiments and modeled for the same level of granularity, same metrics measured, and within the same period, etc.

When using time-series validation: we have train, validation, and test metrics.

## Response decomposition

Incremental volume contributions from each independent variable modeled. This chart shows an example of contributions to sales vs. all other variables that were included in this model.

For example, in the chart shown, 4.3% for TV means that 4.3% of the total sales is being driven by TV, which was 12.4M in sales, considering the modeling date range.

## Aggregated baselines 

Sometimes, baseline variables are hard to interpret, harder to explain. We can get very negative variables that, when summed with very positive ones, will return a baseline value that makes sense.

Use the “Aggregate baseline variables” tool to aggregate several variables into a single summed “Baseline” variable. Baseline can be defined in several different ways, grouping specific variables in this order:

1. Intercept (L1)
2. & Trend (L2)
3. & Time Series Decomp (monthly, weekly, seasonality, yearly...) (L3)
4. & Contextual variables (L4)
5. & Organic variables (L5)

Note: This is a visualization option, it doesn’t affect the model.

## Actual vs Predicted with splits

To have an accurate model, the fit of the modelled data must be accurately relative to the actual data provided. Strive for a high R-squared, where a common rule of thumb is

- R squared < 0.8 = not ideal, aim to improve
- 0.8 < R squared < 0.9 = good
- R squared > 0.9 = really good (beware overfitting*)

Common ways to improve R2 include having a more comprehensive set of independent variables - that is, split up larger paid media channels or include additional baseline (non-media) variables that may explain the dependent variable.

Note: An R2 > 95 may suggest overfitting. If we check the variables and see there is no obvious “cheating”, it’s OK to keep, unless this model will be used for forecasting which requires split validation.

* The separators in the plot will only show up when time series validation (ts_validation) is activated. It adds an additional hyperparameter to split the train and test parts of the data and evaluate if the model is overfitted.

## Revenue Decomposition

Analyze how each variable’s impact varies in time. This plot helps to have a glimpse on when and how much each of the variables that has been included in the model added sales to your brand. The total sum for each period follows the predicted response values calculated.

## Share of Spends vs Share of Effect

The chart shown shows the detailed impact of promotional contributions by comparing several different metrics:

- The share of spend reflects the relative spending of each channel;
- The share of effect is the same as volume contribution i.e. how much incremental sales were driven by each channel;
- ROAS (Return On Advertisement Spend) represents the efficiency of each channel and is calculated by taking the incremental revenue driven by a channel, divided by how much was spent on that channel.

It’s important to consider all metrics before making decisions, as deciding based on one data point might not reflect the whole picture.

## In-cluster ROAS with confidence

We use bootstrapping to calculate uncertainty of ROAS or CPA. The closer to the mean, the more conservative can be. After obtaining all pareto-optimal model candidates, the K-means clustering is applied to find clusters of models with similar hyperparameters. We consider these clusters sub-populations in different local optima. We bootstrap the efficiency metrics to obtain the distribution and 95% CI. The narrower the ranges and nearer the actual ROAS is from the mean ROAS, the more confidence the model is on the reported metric.

## Adstock Decay Rate

This chart represents, on average, the percentage decay rate each channel had. The higher the decay rate, the longer the effect that a specific media channel has after the initial exposure. One way of reading this plot, for TV for example is, 46% of the effect will continue the following week the spend was spent. The third week will have a 46% x 46% (21%), and so on. For geometric adstock, these values are the specific thetas values selected by the algorithm, within your thetas range allowed. For weibull adstock, you will see the shapes and scales. Given this is the most flexible adstock, geometric is one of the curves it contains.

## Immediate vs Carryover

The media contributions are split into immediate contribution and carryover contribution for every promotional channel.

- Immediate is the effect of the direct media spend of a given period.
- Carryover is the effect of historical spend "leaked" (also called adstock) into the given period into following periods.

The carryover effect can be understood as the effect of upper/mid-funnel brand equity metrics on mid/long-term results. In comparison, adstock only addresses media's direct impact on the response and says nothing about the interaction with baseline. Note that it is possible to have a channel with higher carryover but lower adstock; the reason lies in the different saturation curves per media that also have impact on the carryover decomposition. Also, if the beta coefficient is zero, you won’t have carryover nor immediate effect.

## Response Curves

Sometimes called saturation curves or diminishing return, indicate if a specific media channel’s spend is at an optimal level or if it is approaching saturation and therefore suggest potential budget reallocation. The faster the curves reach an inflection point and to a horizontal/flat slope, the quicker the media channel will saturate with each extra ($) spent.
A side-by-side comparison of response curves for each media channel can offer insights into the best opportunities to reallocate spends from more saturated to less saturated channels to improve outcomes. These curves will be used by the budget allocator to find the optimal mix.

Mean theoretical spend per period for a specific channel: mean carryover + mean real spend

## Fitted vs Residuals

This chart shows the relationship between fitted and residual values. For a good fitting model, this plot is expected to show rather horizontal trend line with the dots evenly scattered around the X-axis. On the contrary, visible patterns (funnel shape, waves, grouped outliers) indicate missing variables in the model.

## Export model

Found the best model? Save and report it!

- **Export Selected Model**: Save your model to report, share, replicate.
- **Build PPTX Report**: Auto generate and download a PowerPoint deck with a business-oriented template, showing coverage, results, etc.
- **Brand & Market**: Define the brand and market for your model.
- **Include all files**: You can either download only the JSON file or all the generated plots and CSVs related to the selected model.
- **Report as final model**: Add a label to the JSON file to be able to identify final models.
- **Include relevant comments**: Please, add any information that could be valuable to remember or report, especially caveats, data excluded, and relevant decisions.

## Automatic Report Generation

Download a PowerPoint presentation generated automatically with Rosa with a proper reporting structure, with all the relevant plots and metrics for your selected model, containing some initial insights and placeholders for you to complement and add next steps.

## Extras

On the last tab you’ll be able to see additional plots that apply to all models, result of the iterations process. Some of them will contain:

- Hyperparameters regions
- Pareto clusters details and WSS Curve
- Pareto front (Multi-Objective Optimization)
- Time Series Decomposition with prophet
- ROAS/CPA Convergence
- Time Series Validation and Convergence

Also, we can see the current model's raw JSON text before exporting it.

-----

# 7. Budget Allocator

**Find the optimal budget distribution and answer business scenarios**

Time to optimize the media mix using the selected model

## Scenarios

Each scenario answers to a different business question:

- **Maximum response**: Given a total budget and media-level constraints, the allocator calculates the optimum cross-media budget split by maximizing the response. For example: “What's the optimum media split if I have budget X?”
- **Target efficiency (ROAS or CPA)**: Given a target ROAS or CPA and none media-level constraints, the allocator calculates the optimum cross-media budget split and the total budget by maximizing the response. For example: “What's the optimum media split and how much budget do I need if I wanted to hit ROAS X?”

**Note**: The Budget Allocator doesn’t forecast but compares past performance vs. optimized.

Example of other scenario-related questions can we answer:

### Maximum response

- Compared with my past actual performance, what would have been the maximum ROAS with the same budget I spent? How far were we from the optimal response? >5%?
- How many sales would I would have lost if I would have turned a specific channel off?
- How much can I optimize my media mix if I must fix a specific channel’s spend?
- What are the most efficient channels where I should allocate more budget? (High mROAS)
- What are the less effective channels so I can improve them? (Low ROAS)
- If I had +20% budget in the past, how many additional sales would I have gotten?

### Target efficiency

- What is the sweet spot where the next additional 1€ spent will return 1€? (ROAS = 1)
- How many incremental and total sales would I get with an established ROAS?

Remember, these answers will be the best estimates and can be used as directional to validate your business hypotheses based on your selected model.

## Setup Scenarios

For the selected model, let’s run Budget Allocation scenarios

- **Date range**: Select a period you’d like to use as “Business as Usual” to get the means spends from. Note that the “Total budget” will be re-calculated to reflect the total spent on this selected period.
- **Total budget**: Keep default or customize.
- **Constraints**: For each paid channel, you may constrain the minimum and maximum spend per period (days/weeks/months). Set both levers to 0 or same static value to exclude or fix it, spread or close for more and less flexible for the optimizer to move over the curves and find best mix of spends.

To the right of each lever, you’ll see the relative amount both absolute numbers represent compared with the average spend per channel per period.

The **constraint multiplier** will give you a third scenario to see what would have happened if you were X times more flexible with your channels’ constraints.

ROAS/CPA target: Objective metric (ROAS/CPA) we wish to get without any of the channels’ constraints.

**Target hints**: To give the user and idea on how many incremental and total sales a given ROAS translated to, we provide a proxy that recalculates every time you move the lever.

Note that the budget allocator can optimize “only” the paid promotional variables (excludes organic and contextual variables from the mix) given we can NOT control them. The variables that are not paid variables are then part of the baseline.

## Budget Allocator One-Pager

1. Total budget optimization comparison
2. Channel level spend, response & ROAS details
3. Channel level response curves with adstock & two different constrained allocations
4. **Scenarios:**
- Maximum Response
- Target Efficiency (ROAS/CPA)

## Overall Results

### First section: Total budget optimization

Compare total performance improvement of two different allocation constraints conditions to the initial performance. You will find 3 pillars:

- **Initial pillar:** total spend and response from the last 4 weeks of the modelling window. The simulation period can be customized.
- **Bounded pillar:** optimized total response with the same spend under the user-defined channel-level constraints.
- **Bounded x3 pillar:** optimized total response with the same spend under the 3x stretched constraints.

**Summary**: with the same spend level as the last 4 weeks and the benchmark ROAS of 1.46, bounded budget allocation delivers +11% more sales with a ROAS of 1.62, while the 3x larger bound delivers +15.2% sales with 1.68 ROAS.

Remember, these sales/response are INCREMENTAL sales. The total sales expected would be: baseline (contextual & organic variables) + incremental sales (carryover & immediate).

## ROAS vs mROAS

### Second section: Budget allocation per media

This section shows the average media-level comparison between the three pillars.

- **ROAS = total response / raw spend**. We calculate adstock on the raw spend and obtain the total response. In other words, the total response includes the effect from adstock & raw spend. However, while calculating ROAS, we only consider the “real money”, or raw spend, because it’s misleading to consider “theoretical money” as part of the real cost. The higher the ROAS, the more effective the channel. For “units” dependent variables, CPA = raw spend / total units.
- **mROAS = marginal response / marginal spend.** We refer to the response of the “next dollar spend”. It also represents the slope on the saturation curve; the steeper it is, the more efficient. This metric also reflects how saturated the channel at this spend level is.

At a first glance, it’s counterintuitive to see that ooh_S get budget decrease and facebook_S increase, even though ooh_S has higher total initial ROAS of 3.78, while
facebook_S has ROAS of 0.94. We can see that facebook_S has higher mROAS than ooh_S. This is the reason why the allocator “decides” to increase spend on facebook_S and decrease on ooh_S. mROAS is an indicator of the efficiency of additional spend. For example, channels with high ROAS but low mROAS are likely in the saturation phase. So, any additional investment won't likely bring in the return that the initial investments did. Conversely, channels with high ROAS and high mROAS perform well and will likely continue to yield high returns with additional spending.

#### Optimizing spend allocation using mROAS

Example: mean ROAS looked great, but the last 30% of spend brought 11% additional sales.

1. Don’t be deceived by higher average ROAS numbers. **Always consider mROAS** to ensure each additional dollar spent generates positive returns and contributes to your bottom line.
2. It’s not about how your campaigns have performed on average in the past but how your next dollar spent will perform in the future. It means these **curves will evolve** with time to reflect your performing improvements.

#### Optimizing spend allocation using mCPA

When the dependent variable is set to conversions or units, we are optimizing CPA.

In the optimization curve, we now see an exponential instead of a logarithmic trend, and we aim to minimize CPA & mCPA.

mCPA answers “how much more do I have to spend to get 1 more action at this point?”

Example: If you aim for a mCPA of 30 USD or less, if it is below 30 USD, then you can be sure you're making some amount of profit on every sale that you generate. It doesn't matter your overall CPA, if your mCPA is 30 USD or below, you’re making profit.

## Saturation curves

### Third section: Saturation curve per paid channel

The saturation curve of each media variable is visualized separately. The x-axis refers to spend (theoretical + raw) & y-axis the response (revenue, in this case).

There are four average points on each curve:

**1. Carryover** (white point): historical carryover level in the selected simulation period
**2. Initial** (grey point): mean (raw) spend of the selected simulation period
**3. Bounded** (blue point): optimized spend point and its response
**4. Bounded x3 or Unbounded** (gold point): optimized spend point with 3x stretched range

### Budget Allocator: Baseline is constant across scenarios

How can I calculate the total sales on my scenario?

The Budget Allocator only provides the total sales that you got in a past period and how many sales you would have gotten with a different promotional mix. These are incremental sales.

Notice we can only optimize variables that:

1) are comparable between them (same units)
2) can control (we cannot control competitors or seasons)

We can not add organic variables (which we control) to the mix because they are not comparable with paid channels (i.e. number of clicks or emails are different units than currency).

```
Total Sales = Baseline (L5*) + Paid Channels Response**
```

\* L5 Baseline contains Intercept, Trend, Time Series Decomposition variables (monthly,
weekly, seasonality, yearly...), Contextual variables & Organic variables gathered.

** Response is revenue or units depending on the dependent variable used to model.

-----

# 8. Business Engagement
**Read-Outs and So-What’s**

- What’s next based on the insights gathered during MMM project?
- What does a good So-What look like?

Assuming you are confident on the selected model’s accuracy, it’s time to act:

**1. Assessing current resource allocation**

-  Is current allocation optimal or what is MMM outputs making you to deep dive on?

**2. Validating existing hypotheses**

-  Did the MMM answer your initial questions and hypotheses? Aligned with expectations?
-  Has your promotional spend distribution been acceptable in the past? Could it have been +5% better?

**3. Actions to take place based on outputs**

-  Are you able to shift budgets from lower mROAS to higher mROAS channels?
-  Are you including these insights in your Yearly Brand Plans?
-  Can you improve the performance of the lowest performing channels by shifting its delivery strategy?
-  Can you start measuring additional datapoints to improve insights generation in the future?
-  Should you run another MMM with different granularity (i.e. region or activities) to tackle more business questions?
-  Can you work on influencing external factors that add or reduce to your overall impact? (only promo related)
-  Can you improve the spend channels with a new WBS structure that groups elements for MMM?

## Checklist for the Read-Out

Make sure you include the information below for the read-out and align with your team in advance:

1. Use the **data dictionary** to name the channels in a standard manner

2. Include **descriptive analysis** where needed

3. Include **comparison of full MMM period** analysed **vs previous full year** spend allocation

4. Use **the budget allocator** with **year's budget** (Maximum Response Scenario).

5. Include **comparison of year's budget** with MMM output

6. Address these **3 types of insights:**
  
    a) Validations or hypotheses confirmation
  
    b) Unexpected results (include additional deep dive on results needed)
  
    c) Changes or Watch-outs for Brand Plans 2026

Recommended steps for market to prepare for Read-out sessions

-  If applies: Market visit **follow-ups** (i.e. data preparation) to be finalized by markets

-  **Select single model** for your brand
  -  Select model in market
  -  Validate with MMM experts on time
  -  Share/store final JSON file
  -  Preview with stakeholders and align potential So What's

**Model Read-outs**
  -  Circulate pre-reads 2 days in advance
  -  Structure: 20 mins presentation per brand + 10 mins Q&A

## Expectations

What to expect and prepare for the final Read-outs?

**Expect focus on:**

- Hypotheses tested and reasoning behind relevant assumptions and decisions
- Insights obtained from MMM end-to-end project for brands in scope
- Business actions based on results and insights to implement in coming months
- MMM Adoption and how will it be used in the future → Tweaks for next phases if needed

**Do not expect focus on:**

- Learnings and data challenges → already discussed with the team (ideal)

## Recommended Structure

**In market Read outs guidance:**

- Length: 20 mins per Brand + buffer
- Facilitator: Champion (to set up the meeting)
- Audience: All stakeholders
- Content: full read out presentation (model results exported in Rosa Power Point’s feature) with comments and conclusions aligned with Brand teams.

**Above Market Read outs:**

- Content: Summarized consolidated overview of key results and showcasing of resource
allocation business decisions considered / aligned in market for each Brands

### Remember, this is a journey...

# 9. Other Functionalities

There are additional functionalities and tools like the AI Assistant, recreating & refreshing models, dashboard tool, backlog, etc...

## Dashboard

Compare multiple models with our Dashboard tab. When recreating models an additional tab is enabled: **Dashboard**. To show this dashboard you will need to either:

a) Upload a single JSON file (model)
b) Upload multiple JSON files (models)
c) Select a pre-built and stored project below

Then, you’ll be able to see and compare:

- Summary metrics
- Model Information
- Channels Performance
    - Play with the date range to consider
    - (Details in next slide)
- Model Errors

For every model and date range selected, we will have the following values:

- Every paid and organic variable independently
- PROMOTIONAL TOTAL (sum of paid and organic variables results)
- BASELINE (sum of non-paid nor organic variables)
- GRAND TOTAL (sum of all variables)

Some of the metrics reported are:

- Performance (ROAS / CPA)
- Spend (same currency as data provided)
- Response (calculated sales / conversions)
- Total Contribution (out of 100 sales, how much can be attributed to that specific group)

## Refresh a model

After a model is built and selected, the refresh functionality can be used to continuously build model refreshes when new data arrives. This capability enables MMM to be a continuous reporting tool on a regular frequency.

### When to build a new model instead of refreshing?

When refreshing a model, no need to manually select the best model as before

- **Mostly new data** If the initial model has 100 weeks and there is 80 weeks worth of new data to be added as a refresh, it might be better to rebuild the model.
- **Addition of new variables** If new variables need to be added, then the model design will need to be changed and hence it is better to rebuild the model.

## Dataset AI

There's an AI tool that allows the users to **interact with the dataset**. You can either ask it about the data (i.e. how many rows or columns, average of all spend variables, etc) or interact with the data (i.e. filter data since X, add a new column with this copy/paste data from Excel or aggregate two columns, etc.).

## Rosa AI Assistant

Rosa AI answers to the best of its knowledge any Marketing Mix Model (MMM) or Rosa related questions you may have. Interact with it as you would interact with any real-life expert, using your own language. Be sure to ask relevant on-topic questions to get the most out of it. You can even ask something about your selected model!

### Most frequent use cases asked to the AI Assistant:

**1.Technical Explanations and Definitions**

- Explain key concepts like CPA, mROAS, adstock, hyperparameters.
- Describe the calculation methods and interpretation of various metrics like NRMSE, adjusted R-squared, and mROAS.
- Discuss the significance and implications of different modeling elements, such as response curves, saturation curves, and budget allocation scenarios.
    
**2.Modeling and Data Analysis**

- How to select, apply, and interpret the best models and their results.
- Methods to adjust and optimize model parameters and components.
- Approaches to improve model accuracy and convergence, including dealing with outliers and correlations.

**3.Best Practices and Requirements**

- Share industry best practices for implementing and optimizing marketing mix models (MMM).
- Outline the minimum dataset requirements and key variables to consider for effective MMM.
- Guidance on excluding certain costs and categorizing them appropriately.

**4.Instructional and Educational**

- Provide simplified explanations of complex concepts suitable for different age groups or levels of understanding.
- Design questions and exercises to evaluate and enhance user knowledge on MMM and related topics.
- Offer step-by-step instructions and examples for performing specific tasks or analyses.

**5.Multilingual Requests**

- Translate and explain key concepts and terms in different languages, such as German or Italian.
- Simplify technical explanations to make them more accessible in various languages.

# 10. Resources

- **Rosa Documentation** (this document)
- (Private) GitHub [repository](https://github.com/laresbernardo/Rosa) for R package
- **[Robyn](https://facebookexperimental.github.io/Robyn/docs/welcome/) external documentation**
- **Rosa AI Assistant**
- Contact Bernardo Lares: laresbernardo@gmail.com

<!--
Document variables:

Assigned:
<author> = Vikesh Ajith
<github_username> = SpikyClip

Unassigned (replace all with desired value):
<service-fees-shinyapp>
<screenshot_link>

-->

# Service Fee Statistics

[![GitHub Stars](https://img.shields.io/github/stars/SpikyClip/service-fees-shinyapp.svg)](https://github.com/SpikyClip/service-fees-shinyapp/stargazers)
[![GitHub Issues](https://img.shields.io/github/issues/SpikyClip/service-fees-shinyapp.svg)](https://github.com/SpikyClip/service-fees-shinyapp/issues)
[![Tag](https://img.shields.io/github/v/tag/SpikyClip/service-fees-shinyapp)](https://github.com/SpikyClip/service-fees-shinyapp)
[![License](https://img.shields.io/github/license/SpikyClip/service-fees-shinyapp)](https://github.com/SpikyClip/service-fees-shinyapp/blob/master/LICENSE)

A Rshiny app for comparing standardised service fees.

![Screenshot](img\screenshot.png)

## Introduction

This Rshiny app was developed to compare the standardised price of various
services across competitors in order to decide if service prices should be
raised or lowered to remain competitive.

To use this tool, download the [template csv file](service_fees_app\data\service_fees_template.csv) and fill in the details in the
`Service Provider`, `Service Category`, `Actual Service Duration (mins)` and
`Actual Fee ($)` columns. Upload the csv, and select the reference service
provider.

As each service provider may spend more or less time on a service than others,
the `Standard Fees` setting normalises fees by `($/min)` against the reference
service providers service times. This makes it easier to compare fees across
service providers.

## Additional Information

### License

This project is licensed under the terms of the [MIT license](https://github.com/SpikyClip/service-fees-shinyapp/blob/master/LICENSE).
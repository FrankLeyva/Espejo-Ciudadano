# Analytics System for Espejo Ciudadano Dashboard

## Overview

The analytics system tracks user interactions, session data, and provides insights into dashboard usage. It's designed to be lightweight and privacy-friendly.

## Features

- **Session Tracking**: Records user sessions with start/end times
- **Navigation Tracking**: Tracks which sections users visit
- **Interaction Tracking**: Records user actions (downloads, clicks, etc.)
- **Enhanced Statistics Dashboard**: Secret modal with detailed insights
- **Session Insights**: Duration distribution, peak hours, daily trends
- **Section Insights**: Engagement scores, bounce rates, popular sections
- **Data Export**: Export analytics data in various formats

## How to Access Analytics

### Method 1: Keyboard Shortcut (Recommended)
Press `Ctrl+Shift+A` anywhere on the dashboard

### Method 2: Triple Click Title
Triple-click the "Vista Rápida" title on the main page

### Method 3: Triple Escape
Press the Escape key three times quickly

### Method 4: Console Command
Open browser console and type: `showAnalytics()`

### Method 5: Hidden Footer Trigger
Click the dot (•) in the footer 5 times quickly

## Analytics Data Collected

### Session Data
- Session ID
- Start/end times
- IP address (anonymized)
- User agent
- Total sections visited
- Total downloads

### Navigation Data
- Section visited
- Time spent in section
- Year selected
- Timestamp

### Interaction Data
- Interaction type (download, click, etc.)
- Section where interaction occurred
- Additional details
- Timestamp

## Enhanced Insights

### Session Insights
- **Duration Distribution**: Breakdown of session lengths (Under 1 min, 1-5 min, 5-15 min, 15-30 min, Over 30 min)
- **Peak Usage Hours**: Most active hours of the day
- **Daily Activity Trends**: Sessions and average duration per day
- **Session Patterns**: Understanding user behavior patterns

### Section Insights
- **Engagement Scores**: Calculated as (Average Time × Visits) / 1000
- **Bounce Rate**: Percentage of sessions that only visited one section
- **Most Engaging Sections**: Sections with highest average time spent
- **Popular Sections**: Most visited sections with detailed metrics
- **Unique Visitors**: How many different users visited each section

### Performance Metrics
- **Cache Hit Rate**: How often cached data is used
- **Cache Size**: Memory usage for performance optimization
- **Response Times**: System performance indicators

## Database Schema

The analytics data is stored in SQLite (`data/analytics.db`) with the following tables:

- `sessions`: Session information
- `page_views`: Navigation tracking
- `interactions`: User interactions
- `daily_stats`: Daily summary statistics

## Privacy

- No personally identifiable information is collected
- IP addresses are stored but not displayed in the UI
- Data is stored locally on the server
- Users can access their own session data through the analytics modal

## Testing

Run the test script to verify the analytics system:

```r
source("test_enhanced_analytics.R")
```

This will create test data and verify all enhanced analytics features work correctly.

## Troubleshooting

### Common Issues

1. **"Analytics not initialized"**
   - Check that the `data/` directory exists
   - Verify RSQLite and DBI packages are installed

2. **"Database error"**
   - Check file permissions for `data/analytics.db`
   - Verify SQLite is working properly

3. **Modal doesn't appear**
   - Check browser console for JavaScript errors
   - Verify the trigger methods are working

### Debug Mode

Enable debug logging by checking the browser console for messages starting with:
- `🎯 Analytics triggered:`
- `📊 Analytics trigger system ready`

## API Reference

### AnalyticsManager Class

```r
# Initialize
analytics <- AnalyticsManager$new("path/to/database.db")

# Start session
analytics$start_session(session_id, ip_address, user_agent)

# Track navigation
analytics$track_navigation(session_id, section, year)

# Track interaction
analytics$track_interaction(session_id, type, section, details)

# Get statistics
stats <- analytics$get_stats(days = 30)

# Export data
analytics$export_data(format = "json", file_path = NULL)

# End session
analytics$end_session(session_id)
```

## Configuration

The analytics system can be configured by modifying the `AnalyticsManager` class in `R/analytics.R`:

- Database path
- Data retention period
- Session timeout
- Privacy settings

## Maintenance

### Data Cleanup

Old analytics data is automatically cleaned up, but you can manually clean data older than 365 days:

```r
analytics$cleanup_old_data(days_to_keep = 365)
```

### Daily Summaries

Generate daily summary statistics:

```r
analytics$generate_daily_summary(target_date = Sys.Date() - 1)
```

## Support

For issues with the analytics system, check:
1. Browser console for JavaScript errors
2. R console for R errors
3. Database file permissions
4. Package dependencies 
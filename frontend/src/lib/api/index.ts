// Ancient Compute - API Exports
export { moduleApi, timelineApi, healthApi } from './client';
export type { Module, TimelineEvent, ApiError } from './client';

// Timeline-specific exports
export {
  fetchTimeline,
  fetchEras,
  fetchEraDetail,
  fetchMilestones,
  fetchEraMilestones,
  searchTimeline,
  fetchCivilizations,
  fetchTimelineStats,
  formatYear,
  formatYearRange,
  getEraDuration,
  isYearInEra,
  findEraByYear,
} from './timeline';

export type {
  Milestone,
  TimelineEra,
  EraDetail,
  TimelineFilter,
  FullTimeline,
} from './timeline';

// Education-specific exports
export { default as educationApi } from './education';
export * from './education';

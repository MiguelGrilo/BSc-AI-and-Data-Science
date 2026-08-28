package com.app.meals.ui.screens

import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.Surface
import androidx.compose.runtime.Composable
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.tooling.preview.Preview
import com.app.meals.R
import com.app.meals.model.Meal
import com.app.meals.ui.components.ErrorScreen
import com.app.meals.ui.components.LoadingScreen
import com.app.meals.ui.components.MealsList
import com.app.meals.ui.theme.MealsAppTheme
import com.app.meals.ui.viewModels.DetailsUiState
import com.app.meals.ui.viewModels.HomeUiState
import com.app.meals.ui.viewModels.SearchUiState

/**
 * Mock Data for Previews
 */
@Composable
fun getMockMeal(idSuffix: Int = 0): Meal {
    return Meal(
        id = stringResource(R.string.mock_id) + idSuffix.toString(),
        name = stringResource(R.string.mock_name),
        ingredients = listOf(
            stringResource(R.string.mock_ingredient),
            stringResource(R.string.mock_ingredient),
            stringResource(R.string.mock_ingredient)
        ),
        measures = listOf(
            stringResource(R.string.mock_measure),
            stringResource(R.string.mock_measure),
            stringResource(R.string.mock_measure)
        ),
        category = stringResource(R.string.mock_category),
        country = stringResource(R.string.mock_country),
        instructions = stringResource(R.string.mock_instructions),
        imageUrl = stringResource(R.string.mock_image),
        videoUrl = stringResource(R.string.mock_video),
        source = stringResource(R.string.mock_source)
    )
}

/**
 * Components Previews
 */
@Preview(showBackground = true, name = "Meals List Grid")
@Composable
fun MealsListPreview() {
    MealsAppTheme {
        val mockData = List(6) { index -> getMockMeal(idSuffix = index) }
        MealsList(
            meals = mockData,
            isRefreshing = false,
            onRefresh = {},
            onMealClick = {}
        )
    }
}

/**
 * Home Screen Previews
 */
@Preview(showBackground = true, name = "Home - Loading")
@Composable
fun LoadingScreenPreview() {
    MealsAppTheme {
        LoadingScreen()
    }
}

@Preview(showBackground = true, name = "Home - Error")
@Composable
fun ErrorScreenPreview() {
    MealsAppTheme {
        ErrorScreen(retryAction = {})
    }
}

@Preview(showBackground = true, name = "Home - Success")
@Composable
fun HomeScreenSuccessPreview() {
    MealsAppTheme {
        HomeScreen(
            homeUiState = HomeUiState.Success(List(4) { index -> getMockMeal(idSuffix = index) }),
            onRefresh = {},
            retryAction = {},
            onMealClick = {}
        )
    }
}

/**
 * Search Screen Previews
 */
@Preview(showBackground = true, name = "Search - Idle")
@Composable
fun SearchScreenIdlePreview() {
    MealsAppTheme {
        SearchScreen(
            searchUiState = SearchUiState.Idle,
            searchQuery = "",
            onQueryChange = {},
            onSearchClick = {},
            categories = listOf(
                stringResource(R.string.cat_beef),
                stringResource(R.string.cat_chicken),
                stringResource(R.string.cat_vegan)
            ),
            selectedCategory = stringResource(R.string.cat_beef),
            onCategoryClick = {},
            onMealClick = {}
        )
    }
}

@Preview(showBackground = true, name = "Search - Success")
@Composable
fun SearchScreenSuccessPreview() {
    MealsAppTheme {
        SearchScreen(
            searchUiState = SearchUiState.Success(List(2) { index -> getMockMeal(idSuffix = index) }),            searchQuery = "Chicken",
            onQueryChange = {},
            onSearchClick = {},
            categories = listOf("Beef", "Chicken", "Vegan"),
            selectedCategory = "Chicken",
            onCategoryClick = {},
            onMealClick = {}
        )
    }
}

/**
 * Details Screen Preview
 */
@Preview(showBackground = true, name = "Details - Success")
@Composable
fun DetailsScreenSuccessPreview() {
    MealsAppTheme {
        DetailsScreen(
            detailsUiState = DetailsUiState.Success(getMockMeal()),
            retryAction = {},
            onBackClick = {}
        )
    }
}

/**
 * Settings Screen Previews
 */
@Preview(showBackground = true, name = "Settings - Portrait (Light Mode)")
@Composable
fun LightSettingsScreenPortraitPreview() {
    MealsAppTheme(darkTheme = false) {
        Surface(color = MaterialTheme.colorScheme.background) {
            SettingsScreen(
                isDarkMode = false,
                onThemeToggle = {},
                isPortrait = true
            )
        }
    }
}

@Preview(showBackground = true, name = "Settings - Portrait (Dark Mode)")
@Composable
fun DarkSettingsScreenPortraitPreview() {
    MealsAppTheme(darkTheme = true) {
        Surface(color = MaterialTheme.colorScheme.background) {
            SettingsScreen(
                isDarkMode = true,
                onThemeToggle = {},
                isPortrait = true
            )
        }
    }
}

@Preview(showBackground = true, widthDp = 800, heightDp = 400, name = "Settings - Landscape (Dark Mode)")
@Composable
fun SettingsScreenLandscapePreview() {
    MealsAppTheme(darkTheme = true) {
        Surface(color = MaterialTheme.colorScheme.background) {
            SettingsScreen(
                isDarkMode = true,
                onThemeToggle = {},
                isPortrait = false
            )
        }
    }
}
package com.app.meals.ui.screens

import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.PaddingValues
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.lazy.LazyRow
import androidx.compose.foundation.lazy.items
import androidx.compose.foundation.text.KeyboardActions
import androidx.compose.foundation.text.KeyboardOptions
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.Clear
import androidx.compose.material.icons.filled.Search
import androidx.compose.material3.ExperimentalMaterial3Api
import androidx.compose.material3.FilterChip
import androidx.compose.material3.Icon
import androidx.compose.material3.IconButton
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.OutlinedTextField
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.res.dimensionResource
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.input.ImeAction
import com.app.meals.R
import com.app.meals.ui.components.ErrorScreen
import com.app.meals.ui.components.LoadingScreen
import com.app.meals.ui.components.MealsList
import com.app.meals.ui.viewModels.SearchUiState

@Composable
fun SearchScreen(
    searchUiState: SearchUiState,
    searchQuery: String,
    onQueryChange: (String) -> Unit,
    onSearchClick: () -> Unit,
    categories: List<String>,
    selectedCategory: String?,
    onCategoryClick: (String) -> Unit,
    onMealClick: (String) -> Unit,
    modifier: Modifier = Modifier,
    contentPadding: PaddingValues = PaddingValues(dimensionResource(R.dimen.dimen_0))
) {
    Column(
        modifier = modifier
            .fillMaxSize()
            .padding(contentPadding)
    ) {
        // The Header (Search Bar + Chips)
        SearchHeader(
            searchQuery = searchQuery,
            onQueryChange = onQueryChange,
            onSearchClick = onSearchClick,
            categories = categories,
            selectedCategory = selectedCategory,
            onCategoryClick = onCategoryClick
        )

        // The UI State Handling
        Box(modifier = Modifier.fillMaxSize()) {
            when (searchUiState) {
                is SearchUiState.Idle -> IdleScreen()
                is SearchUiState.Loading -> LoadingScreen(modifier = Modifier.fillMaxSize())
                is SearchUiState.Success -> MealsList(
                    meals = searchUiState.meals,
                    isRefreshing = false,
                    onRefresh = {},
                    onMealClick = onMealClick,
                    modifier = Modifier.fillMaxSize()
                )
                is SearchUiState.Error -> ErrorScreen(
                    retryAction = onSearchClick,
                    modifier = Modifier.fillMaxSize()
                )
            }
        }
    }
}

/**
 * Extracted component containing the Search Bar and Category Chips.
 */
@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun SearchHeader(
    searchQuery: String,
    onQueryChange: (String) -> Unit,
    onSearchClick: () -> Unit,
    categories: List<String>,
    selectedCategory: String?,
    onCategoryClick: (String) -> Unit,
    modifier: Modifier = Modifier
) {
    Column(modifier = modifier.fillMaxWidth()) {
        OutlinedTextField(
            value = searchQuery,
            onValueChange = onQueryChange,
            modifier = Modifier
                .fillMaxWidth()
                .padding(horizontal = dimensionResource(R.dimen.dimen_16), vertical = dimensionResource(R.dimen.dimen_4)),
            placeholder = { Text(stringResource(R.string.search_meal)) },
            leadingIcon = { Icon(Icons.Default.Search, contentDescription = stringResource(R.string.search)) },
            trailingIcon = {
                if (searchQuery.isNotEmpty()) {
                    IconButton(onClick = { onQueryChange("") }) {
                        Icon(Icons.Default.Clear, contentDescription = stringResource(R.string.clear))
                    }
                }
            },
            keyboardOptions = KeyboardOptions(imeAction = ImeAction.Search),
            keyboardActions = KeyboardActions(
                onSearch = { onSearchClick() }
            ),
            singleLine = true,
            shape = MaterialTheme.shapes.large
        )

        LazyRow(
            modifier = Modifier
                .fillMaxWidth()
                .padding(horizontal = dimensionResource(R.dimen.dimen_16), vertical = dimensionResource(R.dimen.dimen_8)),
            horizontalArrangement = Arrangement.spacedBy(dimensionResource(R.dimen.dimen_8))
        ) {
            items(categories) { category ->
                FilterChip(
                    selected = (category == selectedCategory),
                    onClick = { onCategoryClick(category) },
                    label = { Text(category) }
                )
            }
        }
    }
}

/**
 * The empty screen displayed.
 */
@Composable
fun IdleScreen(modifier: Modifier = Modifier) {
    Box(
        modifier = modifier.fillMaxSize(),
        contentAlignment = Alignment.Center
    ) {
        Text(
            text = stringResource(R.string.search_or_select),
            style = MaterialTheme.typography.bodyLarge,
            color = MaterialTheme.colorScheme.onSurfaceVariant
        )
    }
}